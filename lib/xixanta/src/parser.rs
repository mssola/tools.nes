use crate::node::{ControlType, NodeBodyType, NodeType, OperationType, PNode, PString};
use crate::opcodes::{CONTROL_FUNCTIONS, INSTRUCTIONS};
use crate::{Error, SourceInfo};
use std::cmp::Ordering;
use std::io::{self, BufRead, Read};
use std::path;

/// The Parser struct holds basic data for the current parsing session.
#[derive(Default)]
pub struct Parser {
    // The current line number.
    line: usize,

    // The current column number.
    column: usize,

    // The offset of the string being evaluated as a "line". Note that this can
    // vary wildly because on recursive expression parsing the line might be
    // slightly different. This property allows us to have a proper value for
    // each iteration.
    offset: usize,

    /// The nodes that have been evaluated for the current parsing session. You
    /// can count on this vector to be filled after calling
    /// `parser::Parser::parse`. Note that it's a vector of vectors to handle
    /// statements which contain a block (e.g. '.macro'). In the end of the
    /// parsing session, though, this vector of vectors is guaranteed to have
    /// len() == 1, which means that there's only one code "layer" in the end
    /// because the other blocks have been closed (or forced to be closed on bad
    /// .end placements).
    nodes: Vec<Vec<PNode>>,

    /// This is a stack of node types that we are expecting for closing the
    /// currently open inner block. This is done this way to guarantee that end
    /// statements (e.g. '.endproc') go with their respective start ones (e.g.
    /// '.proc'), and they don't close another block.
    bodies: Vec<NodeType>,

    /// List of sources that have been parsed for this parsing session. Consume
    /// it after calling `parse` in order to get the list of sources that have
    /// been evaluated. Note that the `source` index on each PNode points to
    /// this list.
    pub sources: Vec<SourceInfo>,

    /// The index of the source that explicitely belongs to this session (and
    /// not subsequent calls done afterwards). Initialized on `parse`.
    current_source: usize,

    /// A stack of "look ahead" levels. That is, a stack of saved
    /// `look_ahead_index` values that functions that deal with having to look
    /// ahead (e.g. 'parse_literal') have to push/pop so 'parse_expression' can
    /// know when to return early.
    look_ahead_levels: Vec<usize>,

    /// The current level of "looking ahead". That is, some expressions like
    /// '#$1 >> 2' have to evaluate to '(#$1) >> 2' instead of '#$(1 >> 2)'.
    /// This can't be known at first glance because literals, for example, don't
    /// have a termination character (i.e. the parser cannot determine where any
    /// given literal expression ends). Hence, in this case, the parser has to
    /// look ahead to know where the literal ends before dealing with further
    /// operators.
    look_ahead_index: usize,

    /// Number that keeps track of random identifiers being generated so far.
    /// This is because certain anonymous identifiers are not to be exposed to
    /// the human eye, and we just care about them being unique. Hence this
    /// counter.
    generated_identifiers: usize,
}

impl Parser {
    /// Parse the input from the given `reader`. You can then access the results
    /// from the `nodes` field. Otherwise, a vector of Error's might be
    /// returned.
    pub fn parse(&mut self, reader: impl Read, source: SourceInfo) -> Result<(), Vec<Error>> {
        let mut errors = Vec::new();

        // Push the sources for the parsing session (note that this list might
        // be initialized by the caller already). The `current_source` is simply
        // the one we push upon initialization.
        self.sources.push(SourceInfo {
            directory: path::absolute(&source.directory).unwrap_or(source.directory),
            name: source.name,
        });
        self.current_source = self.sources.len() - 1;

        // Make sure that there is a first layer of nodes.
        self.nodes.push(vec![]);

        // This is a line parser (i.e. there cannot be statements which span
        // more than one line). Hence, consume the reader line by line and parse
        // each one.
        for line in io::BufReader::new(reader).lines() {
            match line {
                Ok(l) => {
                    if let Err(mut err) = self.parse_line(l.as_str()) {
                        errors.append(&mut err);
                    }
                }
                Err(_) => errors.push(self.parser_error("could not get line")),
            }
            self.line += 1;
        }

        // Are there any more statements which are still waiting for a closing
        // statement? If so, then there's something wrong.
        if !self.bodies.is_empty() {
            errors.push(
                self.parser_error(
                    format!(
                        "expecting a '{}' but there are no more statements",
                        self.bodies.last().unwrap()
                    )
                    .as_str(),
                ),
            );
        }

        // Truncate the nodes level to 1, just in case there were any errors on
        // not closing a .macro statement or something similar.
        self.nodes.truncate(1);

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Returns the first layer of nodes that have been parsed. Note that this
    /// function only makes sense to be called whenever parsing is done,
    /// otherwise results will be incomplete in (most probably) unexpected ways.
    pub fn nodes(&self) -> Vec<PNode> {
        self.nodes.first().unwrap().to_vec()
    }

    // Parse a single `line` and push the parsed nodes into `self.nodes`.
    fn parse_line(&mut self, line: &str) -> Result<(), Vec<Error>> {
        self.column = 0;
        self.offset = 0;

        // Skip until the first non-whitespace character. If that's not
        // possible, then it's an empty line and we can return early.
        if !self.skip_whitespace(line) {
            return Ok(());
        }

        // Let's pin point the last character we need to care for parsing. This
        // can be either the start position of an inline comment (i.e. ';'), or
        // the real line end. Either way, let `find_line_end` figure this out.
        let end = self.find_line_end(line)?;

        // It's safe to trim the end of the resulting string. Moreover, doing so
        // can already show lines which are actually empty (e.g. a line which
        // simply contains a comment). If this is the case, just return an empty
        // node.
        let mut l = line.get(self.column..end).unwrap_or_default().trim_end();
        if l.is_empty() {
            return Ok(());
        }

        // Fetch the first element of the line, which we will call it an
        // "identifier" but might be a label or a statement. The label might be
        // followed by more code. Hence, push it first, then fetch the next
        // identifier and finally fall through.
        self.offset = 0;
        let (mut id, mut nt) = self.parse_identifier(l, true)?;
        if nt == NodeType::Label {
            self.nodes.last_mut().unwrap().push(PNode {
                node_type: nt,
                value: id,
                left: None,
                right: None,
                args: None,
                source: self.current_source,
            });

            self.skip_whitespace(l);

            // Is it the label alone? If so return early.
            l = line.get(self.column..end).unwrap_or_default().trim_end();
            if l.is_empty() {
                return Ok(());
            }

            // The label is followed by a statement. Let's parse the identifier
            // for it and fall through.
            self.offset = 0;
            (id, nt) = self.parse_identifier(l, true)?;
            if nt == NodeType::Label {
                return Err(self
                    .parser_error("cannot have multiple labels at the same location")
                    .into());
            }

            self.skip_whitespace(l);
        }

        self.parse_statement(l, id)
    }

    // Returns the index to the absolute end of the semantic line (i.e.
    // everything before the end of the string or an inline comment).
    fn find_line_end(&mut self, line: &str) -> Result<usize, Error> {
        // If there is no semicolon, then we just return the end of the line.
        let Some(sc) = line.find(';') else {
            return Ok(line.len());
        };

        // If there is no double quote, then the semicolon that we found is
        // really an inline comment.
        let Some(quote) = line.find('"') else {
            return Ok(sc);
        };

        // If the quote character is after the semicolon, then it's inside of
        // the comment.
        if quote > sc {
            return Ok(sc);
        }

        // Try to fetch the end of the string. If this is not possible, then
        // it's unclosed.
        let Some(mut end_quote) = line.get(quote + 1..).unwrap_or_default().find('"') else {
            return Err(self.parser_error("unclosed string"));
        };

        // If it's there, then end_quote is in relation to 'quote'. Hence, for
        // the absolute value, sum with 'quote' + 2 quote characters. With that,
        // if it's passed the semicolon, then the semicolon is contained inside
        // of a double-quoted string, otherwise stick with the semicolon.
        end_quote += quote + 2;
        if end_quote > sc {
            Ok(end_quote)
        } else {
            Ok(sc)
        }
    }

    // Given a `line` parses an identifier if possible. This identifier can
    // optionally allow a '.' character at the first position if `dot` is set to
    // true. This is usually a good idea if we can have a label or a control
    // statement. Returns a PString representing this identifier on success,
    // plus a hint on whether the identifier belongs to a label or not.
    fn parse_identifier(&mut self, line: &str, dot: bool) -> Result<(PString, NodeType), Error> {
        let start = self.column;
        let base_offset = self.offset;
        let mut nt = NodeType::Value;
        let mut first_seen = false;

        // For the general case we just need to iterate until a whitespace
        // character or an inline comment is found. Then our PString object is
        // merely whatever is on the column..self.column range. Note that we
        // need this iteration to be peekable so we can look ahead. This is
        // interesting for detecting identifiers which are scoped (e.g.
        // "Scope::Identifier").
        let mut chars = line
            .get(self.offset..)
            .unwrap_or_default()
            .chars()
            .peekable();
        while let Some(c) = chars.next() {
            // Error out if '.' is not allowed from now on.
            if c == '.' && (!dot || first_seen) {
                return Err(self.parser_error("cannot have a '.' in this context"));
            }
            first_seen = true;

            // Check for the end of the identifier. For this, it's easier to
            // simply list what is allowed and negate it.
            if !(c.is_ascii_alphanumeric() || c == '@' || c == '_' || c == '.') {
                // This next match looks scarier than what it actually is. To
                // sum things up, the ':' character is quite troublesome, since
                // it can mean three things depending on the context.
                //
                //  1. A label in the form of "label:". In this case there is no
                //     "next" character.
                //  2. A scope operator in "Scope::Variable". In this case we
                //     should swallow up both colon characters and continue
                //     parsing the identifier.
                //  3. A relative label in "jmp :-" or "jmp :+". In this case we
                //     want to exhaust the character stream
                match chars.peek() {
                    Some(pc) => {
                        if c == ':' {
                            let next = *pc;
                            match next {
                                // Scope operator (e.g. "Scope::Variable").
                                ':' => {
                                    chars.next();
                                    self.next();
                                    self.next();
                                    continue;
                                }
                                // Relative label (e.g. "jmp :+" or "jmp :-").
                                '+' | '-' => {
                                    // Relative labels start with ':' *always*.
                                    // Hence, if there was something behind it,
                                    // there is something wrong (e.g. "Bad:+").
                                    if base_offset != self.offset {
                                        return Err(Error {
                                            line: self.line,
                                            global: false,
                                            source: self.sources[self.current_source].clone(),
                                            message: "you cannot have a relative label inside of an identifier".to_string(),
                                    });
                                    }

                                    // Skip the ':' character for the next loop.
                                    self.next();

                                    // Let's exhaust the stream of characters.
                                    let mut size = 0;
                                    for cc in chars.by_ref() {
                                        //  Only four levels of relative jumps
                                        //  are allowed. This is a rather random
                                        //  number, but if you are dealing with
                                        //  more than two levels of relative
                                        //  jumps you are already screwed, so we
                                        //  are actually quite generous here for
                                        //  what it's most probably just
                                        //  spaghetti code.
                                        size += 1;
                                        if size > 4 {
                                            return Err(Error {
                                                line: self.line,
                                                global: false,
                                                source: self.sources[self.current_source].clone(),
                                                message: "you can only jump to a maximum of four relative labels".to_string(),
                                            });
                                        }

                                        // You cannot mix '+'/'-' characters
                                        // with others.
                                        if cc != next {
                                            let msg =
                                                if next == '+' { "forward" } else { "backward" };
                                            return Err(Error {
                                                line: self.line,
                                                global: false,
                                                source: self.sources[self.current_source].clone(),
                                                message: format!("{} relative label can only have '{}' characters", msg, next),
                                            });
                                        }
                                        self.next();
                                    }
                                }
                                // Regular label (e.g. "label:").
                                _ => nt = NodeType::Label,
                            }
                        }
                    }
                    // If there are no characters left, check if it was a regular label.
                    None => {
                        if c == ':' {
                            nt = NodeType::Label;
                        }
                    }
                }

                // The value of the identifier is whatever we have picked up
                // along the parsing.
                let value = String::from(line.get(base_offset..self.offset).unwrap_or("").trim());
                if value == "." || value == "@" || value == "_" {
                    return Err(self.parser_error("empty identifier"));
                }

                // The end of the identifier range has to be shortened for
                // regular labels because we want to ignore to extra ':'
                // character in the end.
                let end = match nt {
                    NodeType::Label => {
                        self.next();
                        self.column - 1
                    }
                    _ => self.column,
                };

                // And we are done.
                return Ok((
                    PString {
                        value,
                        line: self.line,
                        start,
                        end,
                    },
                    nt,
                ));
            }

            self.next();
        }

        // The line is merely the identifier (e.g. instruction with implied
        // addressing).
        let id = String::from(line.get(base_offset..).unwrap_or_default().trim());
        if id == "." || id == "@" || id == "_" {
            return Err(self.parser_error("empty identifier"));
        }
        Ok((
            PString {
                value: id,
                line: self.line,
                start,
                end: self.column,
            },
            NodeType::Value,
        ))
    }

    // Parse the top-level statement as found on the given `line` which has a
    // leading `id` positioned-string which may be an identifier.
    fn parse_statement(&mut self, line: &str, id: PString) -> Result<(), Vec<Error>> {
        // There are only two top-level statements: instructions and
        // assignments. Other kinds of expressions can also be used in the
        // middle of assignments or instructions, and so they have to be handled
        // as common expressions. Whether expressions make sense at the
        // different levels is something to be figured out by the assembler.
        match INSTRUCTIONS.get(&id.value) {
            Some(_) => Ok(self.parse_instruction(line, id)?),
            None => {
                // Skip whitespaces and check if we have a '=' sign. If that's
                // the case, then it's an assignment.
                self.skip_whitespace(line);
                if line.chars().nth(self.offset).unwrap_or_default() == '=' {
                    Ok(self.parse_assignment(line, id)?)
                } else {
                    self.parse_other(line, id)
                }
            }
        }
    }

    // Parse the given `line` as an instruction being identified by `id`.
    fn parse_instruction(&mut self, line: &str, id: PString) -> Result<(), Error> {
        let mut paren = 0;

        // After the initial instruction identifier (e.g. `lda`), there might be
        // an undefined white space. Let's skip it now.
        self.skip_whitespace(line);

        // After skipping the identifier, are we actually on a weird assignment
        // scenario? (e.g. `lda = #42`). If so, then complain on the programmer
        // using a reserved instruction mnemonic as a variable name.
        if line.chars().nth(self.offset).unwrap_or_default() == '=' {
            return Err(self.parser_error(
                format!(
                    "cannot use the reserved mnemonic '{}' as a variable name",
                    id.value
                )
                .as_str(),
            ));
        }

        // If the line at the current point starts with an open paren, then we
        // assume that the indirect addressing mode is being used. If this is
        // the case, then the left arm is actually what's inside of the
        // parenthesis. Otherwise we have to grab until the very end.
        let indirect = line.chars().nth(self.offset).unwrap_or(',') == '(';
        let l = if indirect {
            // Skip the '(' character and skip whitespaces.
            self.next();
            self.skip_whitespace(line);

            // Now let's find the matching paren for the one that opened the
            // indirect addressing mode, and that's the end of our left arm for
            // this instruction.
            paren = self.find_matching_paren(line, self.offset)?;
            line.get(self.offset..paren).unwrap_or_default()
        } else {
            line.get(self.offset..).unwrap_or_default()
        };

        // Is there a left arm at all? If so, then parse it now but considering
        // the trimmed `l` variable, which is a bit special when using indirect
        // addressing mode.
        self.offset = 0;
        let mut left = if l.is_empty() {
            None
        } else {
            Some(Box::new(self.parse_left_arm(l)?))
        };

        // Skip whitespace until the possible right arm. Notice that both
        // `paren` on indirect addressing mode, and `parse_left_arm` have set
        // the "cursor" just after any possible comma. Hence, if there's
        // anything left, then it's the right arm which might have leading
        // spaces.
        self.skip_whitespace(l);

        // At this point, if there is nothing there, then we have no right arm.
        // Otherwise we have to parse the expression.
        let mut right_str = l.get(self.offset..).unwrap_or_default();
        let mut right = if right_str.is_empty() {
            None
        } else {
            self.offset = 0;
            Some(Box::new(self.parse_expression(right_str, 0)?))
        };

        // If we were in indirect addressing mode, then there's some juggling we
        // have to do for the parsed expressions. This is the most complex part
        // from this function, as it will mutate the `left` and the `right`
        // nodes in subtle ways. But this is better than having things in a
        // different function because the rest of the code is pretty much the
        // same.
        if indirect {
            // In indirect addressing mode there's *always* a left arm.
            if left.is_none() {
                return Err(self.parser_error("empty indirect addressing"));
            }

            // Now, here's the trick: if there's something left after the
            // parenthesis, then we have the right arm there. Note that the
            // Indirection node cannot have a right arm at the same time. If
            // this is the case, we are going to freak out now.
            right_str = line.get(paren..).unwrap_or_default();
            if !(right_str.is_empty() || right_str == ")") && right.is_some() {
                return Err(self.parser_error("bad indirect addressing"));
            }

            // The left arm from an indirect addressing mode is actually the
            // indirection itself.
            left = Some(Box::new(PNode {
                node_type: NodeType::Indirection,
                value: PString::default(),
                left,
                right,
                args: None,
                source: self.current_source,
            }));

            // Do we have anything as a right arm?
            right = if right_str.is_empty() || right_str == ")" {
                None
            } else {
                self.offset = 0;

                // Skip any possible leading space on ")   ,".
                self.next();
                self.skip_whitespace(right_str);

                // Skip any possible leading space after the comma.
                self.next();
                self.skip_whitespace(right_str);

                // And finally parse the right arm for the global instruction.
                Some(Box::new(self.parse_expression(right_str, 0)?))
            };
        }

        // We can push the resulting parsed expressions.
        self.nodes.last_mut().unwrap().push(PNode {
            node_type: NodeType::Instruction,
            value: id,
            left,
            right,
            args: None,
            source: self.current_source,
        });

        Ok(())
    }

    // Parse the given `line` as an assignment statement which declares a
    // variable at `id`.
    fn parse_assignment(&mut self, line: &str, id: PString) -> Result<(), Error> {
        // Ensure that the parsed identifier is a valid one on assignments.
        if let Err(msg) = id.is_valid_identifier(false) {
            return Err(self.parser_error(&msg));
        }
        if id.value.contains('.') {
            return Err(self.parser_error("'.' characters are not allowed for variables"));
        }

        // Skip the '=' sign and any possible whitespaces.
        self.next();
        self.skip_whitespace(line);

        // Parse the expression on the right side of the assignment.
        let rest = line.get(self.offset..).unwrap_or("").trim_end();
        if rest.is_empty() {
            return Err(self.parser_error("incomplete assignment"));
        };
        self.offset = 0;
        let left = self.parse_expression(rest, 0)?;

        // And push the node.
        self.nodes.last_mut().unwrap().push(PNode {
            node_type: NodeType::Assignment,
            value: id,
            left: Some(Box::new(left)),
            right: None,
            args: None,
            source: self.current_source,
        });

        Ok(())
    }

    // Parse statements which are neither an instruction nor an assignment. This
    // includes stuff like control statements or macro calls.
    fn parse_other(&mut self, line: &str, id: PString) -> Result<(), Vec<Error>> {
        // This is either a control statement (i.e. starts with '.') or a macro call.
        let node = if id.value.starts_with('.') {
            self.parse_control(id, line, 0)?
        } else {
            let args = self.parse_arguments(line, 0)?;
            PNode {
                node_type: NodeType::Call,
                value: id,
                left: None,
                right: None,
                args: if args.is_empty() { None } else { Some(args) },
                source: self.current_source,
            }
        };

        // If this was an .include statement we have to handle it now as this is
        // not a regular statement but more like a preprocessor statement which
        // can lead to inner parsing sessions.
        if matches!(
            &node.node_type,
            NodeType::Control(ControlType::IncludeSource)
        ) {
            // Validate that it's a top layer statement.
            if self.nodes.len() > 1 {
                return Err(Error {
                    line: node.value.line,
                    global: false,
                    message: ".include statement cannot be inside of a code block".to_string(),
                    source: self.sources[self.current_source].clone(),
                }
                .into());
            }

            // Before including all the nodes from the referenced file, add the
            // .include statement. This should be ignored by the assembler, but
            // maybe other tools want to make use of it.
            self.nodes.last_mut().unwrap().push(node.clone());

            // And append nodes from the referenced source.
            return self.include_source(&node);
        }

        // The given statement might be a start/end one (e.g. '.proc' and
        // '.endproc'). In these cases there are some things to handle besides
        // pushing the node into the current level of nodes.
        let node_type = node.node_type.clone();
        match node.body_type() {
            NodeBodyType::Starts => {
                // .elsif/.else statements close the previous block by mocking
                // an .endif, and then they start a new one.
                if matches!(
                    node_type,
                    NodeType::Control(ControlType::Elsif) | NodeType::Control(ControlType::Else)
                ) {
                    self.close_body(&node_type, &NodeType::Control(ControlType::EndIf))?;
                }

                self.bodies.push(node_type.closing_type().unwrap());
                self.nodes.last_mut().unwrap().push(node);
                self.nodes.push(vec![]);
            }
            NodeBodyType::Ends => {
                self.close_body(&node_type, &node_type)?;

                // If this is an .endif statement, then we need to fold
                // .if/.elsif/.else statements that have accumulated.
                if matches!(node_type, NodeType::Control(ControlType::EndIf)) {
                    self.fold_if_branches()?;
                }
                self.nodes.last_mut().unwrap().push(node);
            }
            NodeBodyType::None => self.nodes.last_mut().unwrap().push(node),
        }

        Ok(())
    }

    // Fold .if/.elsif/.else statements so each branch of the block is on the
    // `left` node of the previous one. The last layer of nodes will also be
    // truncated accordingly.
    fn fold_if_branches(&mut self) -> Result<(), Vec<Error>> {
        let nodes = self.nodes.last_mut().unwrap();
        let mut count = 0;

        // Just iterate in reverse order from the last node until the .if
        // statement that started the whole .if/.elsif/.else block.
        for idx in (1..nodes.len()).rev() {
            let node = &nodes[idx];
            let next = &nodes[idx - 1];

            // If this is an .if already, then we can quit, otherwise we must
            // ensure that the next node is an .if/.elsif and continue the loop.
            match node.node_type {
                NodeType::Control(ControlType::If)
                | NodeType::Control(ControlType::IfDef)
                | NodeType::Control(ControlType::IfNDef) => {
                    nodes.truncate(nodes.len() - count);
                    return Ok(());
                }
                NodeType::Control(ControlType::Elsif) => {
                    if !matches!(
                        next.node_type,
                        NodeType::Control(ControlType::If)
                            | NodeType::Control(ControlType::IfDef)
                            | NodeType::Control(ControlType::IfNDef)
                            | NodeType::Control(ControlType::Elsif)
                    ) {
                        return Err(self.parser_error("expecting an .if statement").into());
                    }
                }
                NodeType::Control(ControlType::Else) => {
                    if !matches!(
                        next.node_type,
                        NodeType::Control(ControlType::If)
                            | NodeType::Control(ControlType::IfDef)
                            | NodeType::Control(ControlType::IfNDef)
                            | NodeType::Control(ControlType::Elsif)
                    ) {
                        return Err(self.parser_error("expecting an .if statement").into());
                    }
                }
                _ => return Err(self.parser_error("expecting an .if statement").into()),
            }

            // After all the checks are done, folding simply means to assign the
            // left node to the current one.
            count += 1;
            nodes[idx - 1].left = Some(Box::new(nodes[idx].clone()));
        }

        nodes.truncate(nodes.len() - count);
        Ok(())
    }

    // Close the currently open block body with the given `node_type`. The given
    // `node_type` is one that ideally closes the current block, but it might
    // not be necessarily what it was really found, which should be passed as
    // `real_type`. This is so statements such as '.elsif' can also close
    // previous `.if/.elsif` blocks.
    fn close_body(&mut self, real_type: &NodeType, node_type: &NodeType) -> Result<(), Vec<Error>> {
        // Pop out which start statement was last seen and check that it
        // makes sense to the end statement we are parsing now.
        let expected_close = match self.bodies.pop() {
            Some(ec) => ec,
            None => {
                return Err(self
                    .parser_error(format!("unexpected '{}'", real_type).as_str())
                    .into())
            }
        };
        if *node_type != expected_close {
            return Err(self
                .parser_error(
                    format!("expecting '{}', found '{}'", expected_close, node_type).as_str(),
                )
                .into());
        }

        // Note that empty bodies are possible. This is left
        // to the caller (e.g. assembler) to decide whether
        // it makes sense or not.
        let nodes = self.nodes.pop().unwrap();
        self.nodes.last_mut().unwrap().last_mut().unwrap().right = Some(Box::new(PNode {
            node_type: NodeType::ControlBody,
            value: PString::default(),
            left: None,
            right: None,
            args: Some(nodes),
            source: self.current_source,
        }));

        Ok(())
    }

    // Consume the given `node` by assuming it's an `.include` statement. This
    // will in turn produce a new parsing session for the given file if possible
    // and push the parsed nodes from it to our current list.
    fn include_source(&mut self, node: &PNode) -> Result<(), Vec<Error>> {
        // First of all, parse the string as it was given and join it with our
        // working directory. This way we construct the absolute path for the
        // given file.
        let file_path = self.fetch_path_from(node.args.as_ref().unwrap().first().unwrap())?;
        let Some(current_source) = self.sources.get(self.current_source) else {
            panic!("mismatch between the number of sources and the current one");
        };
        let abs_file = current_source.directory.join(file_path);

        // And open the file. This is the object to be passed as a reader for
        // the recursive `parse` call, but it also allows us to construct the
        // SourceInfo for the next session because we need to point to its
        // parent in the file system.
        let path = std::path::Path::new(&abs_file);
        let file = match std::fs::File::open(path) {
            Ok(f) => f,
            Err(e) => {
                return Err(Error {
                    line: node.value.line,
                    global: false,
                    source: current_source.clone(),
                    message: format!("could not open source file '{}': {}", file_path, e),
                }
                .into())
            }
        };
        let Some(parent) = path.parent() else {
            return Err(Error {
                line: node.value.line,
                global: false,
                source: current_source.clone(),
                message: format!(
                    "could not find out the parent directory for file '{}'",
                    file_path
                ),
            }
            .into());
        };

        // Set up a new parsing session, push the current sources, and call
        // `parse`.
        let mut parser = Parser {
            sources: self.sources.clone(),
            ..Default::default()
        };
        parser.parse(
            file,
            SourceInfo {
                directory: parent.to_path_buf(),
                name: path.file_name().unwrap().to_str().unwrap().to_string(),
            },
        )?;

        // If everything was fine on the previous parsing session, append the
        // nodes from it and also grab its sources, since the previous parsing
        // session might have done further parsing iterations of its own.
        self.nodes.last_mut().unwrap().append(&mut parser.nodes());
        self.sources = parser.sources.clone();

        Ok(())
    }

    // Returns a string containing the path being referenced in the given node.
    // The path is assumed to be available directly inside of the value of the
    // PNode.
    fn fetch_path_from<'a>(&mut self, node: &'a PNode) -> Result<&'a str, Error> {
        let value = &node.value.value;

        // Validate the path literal.
        if value.len() < 3 || !value.starts_with('"') || !value.ends_with('"') {
            return Err(Error {
                line: node.value.line,
                global: false,
                source: self.sources[self.current_source].clone(),
                message: format!(
                    "path has to be written inside of double quotes ('{}' given instead)",
                    value,
                ),
            });
        }

        Ok(value[1..value.len() - 1].trim())
    }

    // Parse any possible arguments for the given `line`. The offset is supposed
    // to be at a point where arguments might appear, either between parens or
    // not.
    fn parse_arguments(&mut self, line: &str, level: usize) -> Result<Vec<PNode>, Error> {
        // Skip any possible whitespace before the optional opening paren.
        self.skip_whitespace(line);

        // Scope the end of the argument list. If the arguments are enclosed on
        // parenthesis, take that into account, otherwise we will parse until
        // the end of the cleaned line.
        let paren = line.chars().nth(self.offset).unwrap_or_default() == '(';
        let end = if paren {
            // We have a parenthesis. Skip it.
            self.next();
            self.skip_whitespace(line);

            // The end is actually the matching paren for the current opening
            // one.
            self.find_matching_paren(line, self.offset)?
        } else {
            line.len()
        };

        let mut args = Vec::new();
        let trimmed_str = line.get(..end).unwrap_or_default().trim_end();

        // Having an infinite loop with `break`s inside is admittedly not the
        // cleanest thing ever, but it does its job.
        loop {
            // This looks scarier than it actually is. It first finds the end of
            // the argument. Then it calculates a diff on trimming the end or
            // not. This diff will be used to re-adjust the column after the
            // argument is parsed, so we skip any final spaces.
            let (arg_end, comma) = self.find_left_end(trimmed_str)?;
            let arg_untrimmed = line.get(self.offset..arg_end).unwrap_or_default();
            let arg = arg_untrimmed.trim_end();
            let diff = arg_untrimmed.len() - arg.len();

            // Do we actually have an argument. If not then this is the end of
            // our loop.
            if arg.is_empty() {
                break;
            }

            // Parse the argument, which is trimmed down from the line and hence
            // the offset needs to be reset.
            self.offset = 0;
            args.push(self.parse_expression(arg, level + 1)?);

            // After the parsing is done for the current argument, move both
            // `self.offset` and `self.column` right after the end of the
            // current argument.
            self.offset = arg_end;
            self.column += diff;

            // Was the argument ended by a comma? If so there are more arguments
            // to be parsed. Otherwise we can break the loop if it wasn't
            // catched for whatever reason by the previous check.
            if comma {
                // Skip the comma character and any leading white spaces for the
                // next argument.
                self.next();
                self.skip_whitespace(line);
            } else {
                break;
            }
        }

        Ok(args)
    }

    // Parse the left arm from an instruction and leave `offset` and `column`
    // past the end of it.
    fn parse_left_arm(&mut self, line: &str) -> Result<PNode, Error> {
        let start_column = self.column;

        // We track the start value of the offset and we will keep track of the
        // movement of it on `end`. This allows us to preserve the value on
        // inner calls that might modify the offset value.
        let (end, comma) = self.find_left_end(line)?;

        // Set the offset to 0 since we are constraining the string to be
        // parsed.
        let str = line.get(..end).unwrap_or_default().trim_end();
        self.offset = 0;

        // Parse the expression that we can get from the current offset to the
        // computed end.
        let expr = self.parse_expression(str, 0);

        // Set the `offset` and `column` to the end of the line that is shared
        // with the caller.
        self.offset = end;
        self.column = start_column + end;

        // If there was a comma, then the caller expects this function to move
        // both `offset` and `column` past it.
        if comma {
            self.next();
        }
        expr
    }

    // Find the end position for a "left arm"-like expression. That is, there
    // might be opening/closing parenthesis which need to be balanced. On
    // success it returns the index from within the given `line`, and a boolean
    // which is set to true/false on whether a comma was found.
    fn find_left_end(&self, line: &str) -> Result<(usize, bool), Error> {
        let mut idx = self.offset;
        let mut parens = 0;
        let mut comma = false;

        for c in line.get(self.offset..).unwrap_or_default().chars() {
            if c == ',' {
                if parens == 0 {
                    comma = true;
                    break;
                }
            } else if c == '(' {
                parens += 1;
            } else if c == ')' {
                parens -= 1;
            }

            idx += 1;

            if parens < 0 {
                return Err(self.parser_error("too many closing parenthesis"));
            }
        }
        if parens > 0 {
            return Err(self.parser_error("unclosed parenthesis"));
        }

        Ok((idx, comma))
    }

    // Finds the matching parenthesis which closes the parenthesis that was just
    // opened. The `init` index point to the next character after the opening
    // paren from the given `line`.
    fn find_matching_paren(&self, line: &str, init: usize) -> Result<usize, Error> {
        let mut idx = init;
        let mut parens = 1;

        for c in line.get(init..).unwrap_or_default().chars() {
            if c == '(' {
                parens += 1;
            } else if c == ')' {
                parens -= 1;
            }

            match parens.cmp(&0) {
                Ordering::Equal => return Ok(idx),
                Ordering::Less => return Err(self.parser_error("too many closing parenthesis")),
                Ordering::Greater => {}
            }

            idx += 1;
        }
        if parens > 0 {
            return Err(self.parser_error("unclosed parenthesis"));
        }

        Ok(idx)
    }

    // Parses the given line by assuming it's an expression under parenthesis.
    // This function then grabs whatever is inside of these parenthesis and
    // parses the expression inside of them.
    fn extract_paren_expression(&mut self, line: &str, level: usize) -> Result<PNode, Error> {
        // Skip '(' character and whitespace characters in between.
        self.next();
        self.skip_whitespace(line);

        // Extract the string inside of the enclosing parenthesis.
        let paren = self.find_matching_paren(line, self.offset)?;
        let l = line.get(self.offset..paren).unwrap_or_default();

        // The offset will be reset since we are trimming the string to be
        // parsed. That being said, keep the previous value so it can be added
        // afterwards.
        let prev = self.offset;
        self.offset = 0;

        // Parenthesis reset the index of looking ahead, since ambiguities are
        // removed by using parenthesis. Hence, store the current index and
        // reset it.
        let prev_idx = self.look_ahead_index;
        self.look_ahead_index = 0;

        // Parse the inner expression and restore the 'look_ahead_index'. As for
        // the 'offset', the inner expression has moved it, so add the previous
        // value to it so the value matches the line originally given to this
        // function.
        let node = self.parse_expression(l, level + 1)?;
        self.look_ahead_index = prev_idx;
        self.offset += prev;

        // Skip ')' character and return the parsed expression.
        self.next();
        Ok(node)
    }

    // Consumes the given line by assuming is a string in double quotes.
    fn parse_quoted_string(&mut self, line: &str) -> Result<PNode, Error> {
        let start = self.offset;
        let start_column = self.column;

        // Skip opening quote.
        self.next();

        // Just iterate over the string until we find the matching quote
        // character (unless it was escaped through '\').
        let mut prev = '"';
        for ch in line.get(self.offset..).unwrap_or("").chars() {
            if !ch.is_ascii() {
                return Err(self.parser_error("using non-ASCII characters in a string"));
            }

            if ch == '"' && prev != '\\' {
                self.next();

                return Ok(PNode {
                    node_type: NodeType::Value,
                    value: PString {
                        value: line.get(start..self.offset).unwrap().to_string(),
                        line: self.line,
                        start: start_column,
                        end: self.column,
                    },
                    left: None,
                    right: None,
                    args: None,
                    source: self.current_source,
                });
            }

            self.next();
            prev = ch;
        }

        Err(self.parser_error("unclosed string"))
    }

    // Returns a node for the given `node_type` unary operation, where the given
    // `line` is the whole expression (including the unary operator).
    fn parse_unary_operation(
        &mut self,
        node_type: NodeType,
        line: &str,
        level: usize,
    ) -> Result<PNode, Error> {
        // Save the symbol before discarding it, since it's going to be used for
        // the PString's value.
        let symbol = line.chars().nth(0).unwrap();

        // Skip operator and whitespaces.
        let start = self.column;
        self.next();
        self.skip_whitespace(line);

        // Fetch the right side of the operator.
        let right_str = line.get(self.offset..).unwrap_or_default().trim_end();
        self.offset = 0;
        let right = self.parse_expression(right_str, level + 1)?;

        Ok(PNode {
            node_type,
            value: PString {
                value: String::from(symbol) + &right.value.value,
                line: self.line,
                start,
                end: right.value.end,
            },
            left: None,
            right: Some(Box::new(right)),
            args: None,
            source: self.current_source,
        })
    }

    // Parse the expression under `line`. Indeces such as `self.column` and
    // `self.offset` are assumed to be correct at this point for the given
    // `line` (e.g. the line might not be a full line but rather a limited range
    // and the offset has been set accordingly). Moreover, this function is
    // expected to be called rather often in a recursive manner. In order to
    // avoid stack problems, a `level` of recursivity is passed so we can
    // prevent too many nested expressions before exhausting the call stack.
    // Returns a new node for the expression at hand.
    fn parse_expression(&mut self, line: &str, level: usize) -> Result<PNode, Error> {
        // Avoid stack overflows from too many recursive calls for expressions
        // that are too nested. I have only seen this recursion level on wild
        // inputs while fuzzy testing, so no real human code should reach this
        // point. Eitherway, let's be safe.
        if level >= 16 {
            return Err(self.parser_error("there are too many nested expressions"));
        }

        // Parse the expression that we have here and now.
        let expr = self.parse_expression_at_point(line, level)?;

        // Were we looking ahead? If so simply return so the caller can react to
        // it.
        let starting_level = *self.look_ahead_levels.last().unwrap_or(&0);
        if self.look_ahead_index > starting_level {
            return Ok(expr);
        }

        // We were not looking ahead at this point. Let's grab the rest of the
        // string and check if there is a binary operation at hand.
        self.skip_whitespace(line);
        let l = line.get(self.offset..).unwrap_or_default().trim_end();

        let (op, op_size) = self.get_operation_from_line(l)?;
        if let Some(node_type) = op {
            // The left node of the operator is simply the "identifier" that
            // came to us.
            let left = Some(Box::new(expr));

            // Fetch a trimmed version of the string that comes after the
            // operator.
            self.offset += op_size;
            self.column += op_size;
            self.skip_whitespace(line);
            let right_str = line.get(self.offset..).unwrap_or_default().trim_end();

            // The right node of the operation will be the parsed expression
            // of the string we just got right of the operator.
            let prev = self.offset;
            self.offset = 0;
            let right = self.parse_expression(right_str, level + 1)?;
            self.offset += prev;

            return Ok(PNode {
                node_type,
                value: PString {
                    value: String::from(""),
                    line: left.as_ref().unwrap().value.line,
                    start: left.as_ref().unwrap().value.start,
                    end: right.value.end,
                },
                left,
                right: Some(Box::new(right)),
                args: None,
                source: self.current_source,
            });
        }

        // There is no binary operation. Hence, just return the expression as
        // expected.
        Ok(expr)
    }

    // Parse the expression from the very start of the given `line` and by
    // assuming that there are no further expressions to be parsed after the
    // given one (this is the task for the caller). The `level` of recursivity
    // is also passed since it might internally call `parse_expression` again.
    fn parse_expression_at_point(&mut self, line: &str, level: usize) -> Result<PNode, Error> {
        let first = line.chars().next().unwrap_or_default();

        if first == '(' {
            self.extract_paren_expression(line, level)
        } else if first == '.' {
            let (id, _) = self.parse_identifier(line, true)?;
            self.parse_control(id, line, level)
        } else if first == '\'' {
            self.parse_char(line)
        } else if first == '$' || first == '#' || first == '%' {
            // Literal symbols come with a single character, or with two only on
            // '#$' or '#%'. Other variations are illegal and should be avoided
            // to prevent crashes.
            if let Some(next) = line.chars().nth(1) {
                if next == '#' || (first != '#' && (next == '$' || next == '%')) {
                    return Err(Error {
                        line: self.line,
                        global: false,
                        source: self.sources[self.current_source].clone(),
                        message: "bad literal syntax".to_string(),
                    });
                }
            }

            self.parse_literal(line, first, level)
        } else if let Some(node_type) = self.get_unary_from_line(line) {
            // Only treat this as a unary operator if the next character is not
            // another unary operator (e.g. disambiguate between '<<' and '<').
            if self
                .get_unary_from_line(line.get(1..).unwrap_or(""))
                .is_none()
            {
                self.parse_unary_operation(node_type, line, level)
            } else {
                self.parse_value(line)
            }
        } else {
            self.parse_value(line)
        }
    }

    // Like `parse_identifier` but it returns an error if a label was found.
    fn parse_value(&mut self, line: &str) -> Result<PNode, Error> {
        let (id, nt) = self.parse_identifier(line, true)?;
        if id.is_empty() {
            return Err(self.parser_error("invalid identifier"));
        }

        if nt == NodeType::Label {
            Err(self.parser_error("not expecting a label defined here"))
        } else {
            Ok(PNode {
                node_type: NodeType::Value,
                value: id,
                left: None,
                right: None,
                args: None,
                source: self.current_source,
            })
        }
    }

    // Returns the unary operator type which is at the beginning of the line if
    // it exists, otherwise returns None.
    fn get_unary_from_line(&mut self, line: &str) -> Option<NodeType> {
        match line.chars().nth(0).unwrap_or_default() {
            '+' => Some(NodeType::Operation(OperationType::UnaryPositive)),
            '-' => Some(NodeType::Operation(OperationType::UnaryNegative)),
            '~' => Some(NodeType::Operation(OperationType::BitwiseNot)),
            '<' => Some(NodeType::Operation(OperationType::LoByte)),
            '>' => Some(NodeType::Operation(OperationType::HiByte)),
            '!' => Some(NodeType::Operation(OperationType::LogicalNot)),
            _ => None,
        }
    }

    // Get the operator from the first two characters of the given line. If
    // there's no operator, then None is returned for the first element of the
    // Ok tuple. Moreover, the second item on the tuple contains the amount of
    // characters that made up the operator.
    fn get_operation_from_line(&mut self, line: &str) -> Result<(Option<NodeType>, usize), Error> {
        let first = line.chars().nth(0).unwrap_or_default();
        let second = line.chars().nth(1).unwrap_or_default();

        match first {
            '+' => Ok((Some(NodeType::Operation(OperationType::Add)), 1)),
            '-' => Ok((Some(NodeType::Operation(OperationType::Sub)), 1)),
            '*' => Ok((Some(NodeType::Operation(OperationType::Mul)), 1)),
            '/' => Ok((Some(NodeType::Operation(OperationType::Div)), 1)),
            '^' => Ok((Some(NodeType::Operation(OperationType::Xor)), 1)),
            '|' => match second {
                '|' => Ok((Some(NodeType::Operation(OperationType::LogicalOr)), 2)),
                _ => Ok((Some(NodeType::Operation(OperationType::Or)), 1)),
            },
            '&' => match second {
                '&' => Ok((Some(NodeType::Operation(OperationType::LogicalAnd)), 2)),
                _ => Ok((Some(NodeType::Operation(OperationType::And)), 1)),
            },
            '<' => match second {
                '<' => Ok((Some(NodeType::Operation(OperationType::Lshift)), 2)),
                '>' => Ok((Some(NodeType::Operation(OperationType::NotEqual)), 2)),
                '=' => Ok((Some(NodeType::Operation(OperationType::LessEqual)), 2)),
                _ => Ok((Some(NodeType::Operation(OperationType::Less)), 1)),
            },
            '>' => match second {
                '>' => Ok((Some(NodeType::Operation(OperationType::Rshift)), 2)),
                '=' => Ok((Some(NodeType::Operation(OperationType::GreaterEqual)), 2)),
                _ => Ok((Some(NodeType::Operation(OperationType::Greater)), 1)),
            },
            '=' => match second {
                '=' => Ok((Some(NodeType::Operation(OperationType::Equal)), 2)),
                // 'ca65' allows a single '=' for comparison as well. This is in
                // my humble opinion bad, but existing code uses this, so let's
                // add support for it.
                _ => Ok((Some(NodeType::Operation(OperationType::Equal)), 1)),
            },
            '!' => match second {
                '=' => Ok((Some(NodeType::Operation(OperationType::NotEqual)), 2)),
                _ => Err(self.parser_error(format!("unknown operator '!{}'", second).as_str())),
            },
            _ => Ok((None, 0)),
        }
    }

    // Generate a unique identifier with the given prefix.
    fn unique_identifier(&mut self, prefix: String) -> String {
        let res = prefix + &String::from("-") + &self.generated_identifiers.to_string();
        self.generated_identifiers += 1;
        res
    }

    // Returns a NodeType::Control node with whatever could be parsed
    // considering the given `id` and rest of the `line`.
    fn parse_control(&mut self, id: PString, line: &str, level: usize) -> Result<PNode, Error> {
        let mut left = None;

        // Ensure that this is a function that we know of. In the past this was
        // not done and it brought too many problems that made the more
        // "abstract" way of handling this just too complicated.
        let control = match CONTROL_FUNCTIONS.get(&id.value.to_lowercase()) {
            Some(control) => control,
            None => {
                return Ok(PNode {
                    node_type: NodeType::Value,
                    value: id,
                    left: None,
                    right: None,
                    args: None,
                    source: self.current_source,
                });
            }
        };

        // If this control function has an identifier (e.g. `.macro
        // Identifier(args...)`), let's parse it now.
        if control.has_identifier.is_some() {
            self.skip_whitespace(line);
            left = Some(Box::new(PNode {
                node_type: NodeType::Value,
                // The identifier is actually there or does it have to be generated?
                value: if control.has_identifier.unwrap() {
                    PString {
                        value: self.unique_identifier(control.control_type.to_string()),
                        line: self.line,
                        start: id.start,
                        end: id.end,
                    }
                } else {
                    self.parse_identifier(line, false)?.0
                },
                left: None,
                right: None,
                args: None,
                source: self.current_source,
            }));
        }

        // At this point we reached the arguments (i.e. any identifier required
        // by the control function has already been parsed and set in `left`).
        // Then, just parse the arguments and ensure that it matches the amount
        // required by the function.
        let args = if control.only_string {
            self.skip_whitespace(line);
            vec![self.parse_quoted_string(line)?]
        } else {
            self.parse_arguments(line, level)?
        };
        if let Some(args_required) = control.required_args {
            if args.len() < args_required.0 || args.len() > args_required.1 {
                return Err(self.parser_error(
                    format!("wrong number of arguments for function '{}'", id.value).as_str(),
                ));
            }
        }

        Ok(PNode {
            node_type: NodeType::Control(control.control_type.clone()),
            value: id,
            left,
            right: None,
            args: if args.is_empty() { None } else { Some(args) },
            source: self.current_source,
        })
    }

    // Returns a NodeType::Literal node with whatever could be parsed
    // considering the given `line` which starts with the given `symbol`. The
    // recursivity `level` is also provided as it will call again
    // `parse_expression`.
    fn parse_literal(&mut self, line: &str, symbol: char, level: usize) -> Result<PNode, Error> {
        // Enforce that literal symbols and their values are not separated by
        // random white space characters. Other assemblers (e.g. ca65) also take
        // this stance, and through fuzzy testing I realized that not doing this
        // could result in general bad behavior.
        let inner = line.get(1..).unwrap_or("");
        if let Some(c) = inner.chars().nth(0) {
            if c.is_whitespace() {
                return Err(Error {
                    line: self.line,
                    global: false,
                    source: self.sources[self.current_source].clone(),
                    message: "numeric literals cannot have white spaces".to_string(),
                });
            }
        }

        // Preserve the initial column value and advance it to skip the 'symbol'
        // character. Then we need to reset the offset as usual since the parsed
        // expression has been trimmed.
        let start = self.column;
        self.column += 1;
        self.offset = 0;

        // Parse the inner expression while also pushing/popping the look ahead
        // status. This is needed as there are no closing characters for a given
        // literal, so to disambiguate we must look ahead.
        self.look_ahead_levels.push(self.look_ahead_index);
        self.look_ahead_index += 1;
        let left = self.parse_expression(inner, level + 1)?;
        self.look_ahead_index -= 1;
        self.look_ahead_levels.pop();

        // Add the 'symbol' to the final offset.
        self.offset += 1;

        Ok(PNode {
            node_type: NodeType::Literal,
            value: PString {
                value: String::from(symbol) + &left.value.value,
                line: self.line,
                start,
                end: left.value.end,
            },
            left: Some(Box::new(left)),
            right: None,
            args: None,
            source: self.current_source,
        })
    }

    // Returns a NodeType::Literal node with the given `id` parsed as a
    // character literal. This literal will have on the left node a value which
    // is the character transformed into a decimal value.
    fn parse_char(&mut self, line: &str) -> Result<PNode, Error> {
        if line.len() != 3 {
            return Err(self.parser_error("bad character literal"));
        }

        // Grab the actual character and the delimiter.
        let mut chars = line.chars();
        let del = chars.next().unwrap();
        let ch = chars.next().unwrap();

        // Sanity checks.
        if !ch.is_ascii_alphanumeric() {
            return Err(self.parser_error(
                "only alphanumeric ASCII characters are allowed on character literals",
            ));
        }
        if del != '\'' || chars.next().unwrap() != '\'' {
            return Err(self.parser_error("bad character literal"));
        }

        // Advance the column passed ahead of this literal (note the the PNode
        // below will have to take this into account with the start/end values).
        self.column += 3;

        Ok(PNode {
            node_type: NodeType::Literal,
            value: PString {
                value: format!("'{}'", ch),
                line: self.line,
                start: self.column - 3,
                end: self.column,
            },
            left: Some(Box::new(PNode {
                node_type: NodeType::Value,
                value: PString {
                    value: (ch as u8).to_string(),
                    line: self.line,
                    start: self.column - 2,
                    end: self.column - 1,
                },
                left: None,
                right: None,
                args: None,
                source: self.current_source,
            })),
            right: None,
            args: None,
            source: self.current_source,
        })
    }

    // Returns a new Error by using the current line.
    fn parser_error(&self, msg: &str) -> Error {
        Error {
            message: String::from(msg),
            global: false,
            source: self.sources[self.current_source].clone(),
            line: self.line,
        }
    }

    // Advances `self.column` and `self.offset` until a non-whitespace character
    // is found. Note that the initial index is bound to `self.offset`. Returns
    // false if the line can be skipped entirely, true otherwise.
    fn skip_whitespace(&mut self, line: &str) -> bool {
        if line.is_empty() {
            return false;
        }

        for c in line.get(self.offset..).unwrap_or("").chars() {
            if !c.is_whitespace() {
                if c == ';' {
                    return false;
                }
                return true;
            }

            self.next();
        }

        true
    }

    // Increment `self.column` and `self.offset` by one.
    fn next(&mut self) {
        self.column += 1;
        self.offset += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::node::ControlType;

    fn assert_one_valid(parser: &mut Parser, line: &str) {
        assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());
        assert!(parser.nodes.len() == 1);
    }

    fn assert_node(node: &PNode, nt: NodeType, line: &str, value: &str) {
        assert_eq!(node.node_type, nt);
        assert_eq!(
            node.value.value.as_str(),
            line.get(node.value.start..node.value.end).unwrap()
        );
        assert_eq!(node.value.value.as_str(), value);
    }

    // Empty

    #[test]
    fn empty_line() {
        let mut parser = Parser::default();
        assert!(parser.parse("".as_bytes(), SourceInfo::default()).is_ok());
        assert_eq!(parser.nodes.last().unwrap().len(), 0);
    }

    #[test]
    fn spaced_line() {
        let mut parser = Parser::default();
        assert!(parser
            .parse("   ".as_bytes(), SourceInfo::default())
            .is_ok());
        assert_eq!(parser.nodes.last().unwrap().len(), 0);
    }

    #[test]
    fn just_a_comment_line() {
        for line in vec![";; This is a comment", "    ;; Comment"].into_iter() {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());
            assert_eq!(parser.nodes.last().unwrap().len(), 0);
        }
    }

    // Labels

    #[test]
    fn anonymous_label() {
        let mut parser = Parser::default();
        assert!(parser.parse(":".as_bytes(), SourceInfo::default()).is_ok());

        let mut nodes = parser.nodes.last().unwrap();
        assert_eq!(nodes.len(), 1);
        assert!(nodes.first().unwrap().value.value.is_empty());
        assert_eq!(nodes.first().unwrap().value.start, 0);
        assert_eq!(nodes.first().unwrap().value.end, 0);

        parser = Parser::default();
        assert!(parser
            .parse("  :".as_bytes(), SourceInfo::default())
            .is_ok());

        nodes = parser.nodes.last().unwrap();
        assert_eq!(nodes.len(), 1);
        assert!(nodes.first().unwrap().value.value.is_empty());
        assert_eq!(nodes.first().unwrap().value.start, 2);
        assert_eq!(nodes.first().unwrap().value.end, 2);
    }

    #[test]
    fn named_label() {
        let mut parser = Parser::default();
        assert!(parser
            .parse("label:".as_bytes(), SourceInfo::default())
            .is_ok());

        let mut nodes = parser.nodes.last().unwrap();
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes.first().unwrap().value.value, "label");
        assert_eq!(nodes.first().unwrap().value.start, 0);
        assert_eq!(nodes.first().unwrap().value.end, 5);

        parser = Parser::default();
        assert!(parser
            .parse("  label:".as_bytes(), SourceInfo::default())
            .is_ok());

        nodes = parser.nodes.last().unwrap();
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes.first().unwrap().value.value, "label");
        assert_eq!(nodes.first().unwrap().value.start, 2);
        assert_eq!(nodes.first().unwrap().value.end, 7);
    }

    #[test]
    fn label_with_instruction() {
        let line = "label: dex";

        let mut parser = Parser::default();
        assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

        let nodes = parser.nodes();
        assert_eq!(nodes.len(), 2);

        // Label.
        assert_eq!(nodes.first().unwrap().value.value, "label");
        assert_eq!(nodes.first().unwrap().value.start, 0);
        assert_eq!(nodes.first().unwrap().value.end, 5);

        // Instruction
        assert_node(nodes.last().unwrap(), NodeType::Instruction, line, "dex")
    }

    #[test]
    fn dot_label() {
        let line = ".L1: dex";

        let mut parser = Parser::default();
        assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

        let nodes = parser.nodes();
        assert_eq!(nodes.len(), 2);

        // Label.
        assert_eq!(nodes.first().unwrap().value.value, ".L1");
        assert_eq!(nodes.first().unwrap().value.start, 0);
        assert_eq!(nodes.first().unwrap().value.end, 3);

        // Instruction
        assert_node(nodes.last().unwrap(), NodeType::Instruction, line, "dex")
    }

    // Literals

    #[test]
    fn parse_pound_literal() {
        for line in vec![
            "lda #20",
            " lda  #20 ",
            "  lda   #20   ; Comment",
            "  label:   lda #20",
        ]
        .into_iter()
        {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let nodes = parser.nodes();

            let node = nodes.last().unwrap().left.as_ref().unwrap();
            assert_eq!(node.node_type, NodeType::Literal);
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            let left = node.left.clone().unwrap();
            assert_eq!(left.node_type, NodeType::Value);
            assert_eq!(left.value.value, "20");
            assert_eq!(line.get(left.value.start..left.value.end).unwrap(), "20");
        }
    }

    #[test]
    fn parse_compound_literal() {
        let line = "lda #$20";
        let mut parser = Parser::default();
        assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

        let nodes = parser.nodes();

        let node = nodes.last().unwrap().left.as_ref().unwrap();
        assert_eq!(node.node_type, NodeType::Literal);
        assert!(node.right.is_none());
        assert!(node.args.is_none());

        let inner = node.left.clone().unwrap();
        assert_eq!(inner.node_type, NodeType::Literal);
        assert_eq!(inner.value.value, "$20");
        assert_eq!(line.get(inner.value.start..inner.value.end).unwrap(), "$20");

        let innerinner = inner.left.clone().unwrap();
        assert_eq!(innerinner.node_type, NodeType::Value);
        assert_eq!(innerinner.value.value, "20");
        assert_eq!(
            line.get(innerinner.value.start..innerinner.value.end)
                .unwrap(),
            "20"
        );
    }

    #[test]
    fn parse_variable_in_literal() {
        let line = "lda #Variable";
        let mut parser = Parser::default();
        assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

        let nodes = parser.nodes();

        let node = nodes.last().unwrap().left.as_ref().unwrap();
        assert_eq!(node.node_type, NodeType::Literal);
        assert!(node.right.is_none());
        assert!(node.args.is_none());

        let inner = node.left.clone().unwrap();
        assert_eq!(inner.node_type, NodeType::Value);
        assert_eq!(inner.value.value, "Variable");
        assert_eq!(
            line.get(inner.value.start..inner.value.end).unwrap(),
            "Variable"
        );
    }

    #[test]
    fn parse_paren_expression() {
        let line = "ldx #(Variable)";
        let mut parser = Parser::default();
        assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

        let instr = parser.nodes.last().unwrap().last().unwrap();
        assert_eq!(instr.node_type, NodeType::Instruction);
        assert!(instr.right.is_none());
        assert!(instr.args.is_none());

        let node = instr.left.clone().unwrap();
        assert_eq!(node.node_type, NodeType::Literal);
        assert!(node.right.is_none());
        assert!(node.args.is_none());

        let inner = node.left.clone().unwrap();
        assert_eq!(inner.node_type, NodeType::Value);
        assert_eq!(inner.value.value, "Variable");
        assert_eq!(
            line.get(inner.value.start..inner.value.end).unwrap(),
            "Variable"
        );
    }

    #[test]
    fn parse_bad_literals() {
        for line in vec!["lda #", "lda #%", "lda $"].into_iter() {
            let mut parser = Parser::default();
            let err = parser
                .parse(line.as_bytes(), SourceInfo::default())
                .unwrap_err();

            assert_eq!(err.first().unwrap().message, "invalid identifier");
        }

        for line in vec!["lda $ 2", "lda #% 2", "lda # 2"].into_iter() {
            let mut parser = Parser::default();
            let err = parser
                .parse(line.as_bytes(), SourceInfo::default())
                .unwrap_err();

            assert_eq!(
                err.first().unwrap().message,
                "numeric literals cannot have white spaces"
            );
        }

        for line in vec![
            "lda ##2",
            "lda #$$2",
            "lda $$2",
            "lda #$#$2",
            "lda #$#2",
            "lda ###",
        ]
        .into_iter()
        {
            let mut parser = Parser::default();
            let err = parser
                .parse(line.as_bytes(), SourceInfo::default())
                .unwrap_err();

            assert_eq!(err.first().unwrap().message, "bad literal syntax");
        }
    }

    #[test]
    fn parse_string() {
        let mut parser = Parser::default();
        let line = ".asciiz \"=a: b, c; d\"  ; Comment";

        assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

        let stmt = parser.nodes.last().unwrap().last().unwrap();
        let inner = stmt.args.as_ref().unwrap().first().unwrap();
        assert_eq!(inner.node_type, NodeType::Value);
        assert_eq!(inner.value.value, "\"=a: b, c; d\"");
        assert_eq!(
            line.get(inner.value.start..inner.value.end).unwrap(),
            "\"=a: b, c; d\""
        );
    }

    #[test]
    fn error_on_non_ascii_string() {
        let mut parser = Parser::default();
        let line = ".asciiz \"à\"";

        let err = parser
            .parse(line.as_bytes(), SourceInfo::default())
            .unwrap_err();

        assert_eq!(
            err.first().unwrap().message,
            "using non-ASCII characters in a string"
        );
    }

    // Regular instructions.

    #[test]
    fn instruction_with_implied() {
        for line in vec![
            "dex",
            "    dex",
            " dex  ",
            "  dex   ; Comment",
            "  label:   dex",
        ]
        .into_iter()
        {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Instruction, line, "dex");
            assert!(node.left.is_none());
            assert!(node.right.is_none());
            assert!(node.args.is_none());
        }
    }

    #[test]
    fn instruction_with_implied_explicit() {
        for line in vec!["inc a", "    inc a", " inc  a  "].into_iter() {
            let mut parser = Parser::default();
            assert_one_valid(&mut parser, line);

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Instruction, line, "inc");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            assert_node(&node.left.clone().unwrap(), NodeType::Value, line, "a");
        }
    }

    #[test]
    fn instruction_with_zeropage() {
        for line in vec!["inc $20", "    inc   $20", " inc  $20  "].into_iter() {
            let mut parser = Parser::default();
            assert_one_valid(&mut parser, line);

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Instruction, line, "inc");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            assert_node(&node.left.clone().unwrap(), NodeType::Literal, line, "$20");
        }
    }

    #[test]
    fn instruction_with_immediate() {
        for line in vec!["adc #$20", "  adc #$20  ", "  adc   #$20  "].into_iter() {
            let mut parser = Parser::default();
            assert_one_valid(&mut parser, line);

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Instruction, line, "adc");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            assert_node(&node.left.clone().unwrap(), NodeType::Literal, line, "#$20");
        }
    }

    #[test]
    fn instruction_with_absolute() {
        for line in vec!["inc $2002", "    inc   $2002", " inc  $2002  "].into_iter() {
            let mut parser = Parser::default();
            assert_one_valid(&mut parser, line);

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Instruction, line, "inc");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            assert_node(
                &node.left.clone().unwrap(),
                NodeType::Literal,
                line,
                "$2002",
            );
        }
    }

    #[test]
    fn instruction_with_absolute_x() {
        for line in vec![
            "inc $2002, x",
            "    inc   $2002, x",
            " inc  $2002,    x  ",
            " label:   inc $2002,   x  ; Comment",
        ]
        .into_iter()
        {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Instruction, line, "inc");
            assert!(node.args.is_none());

            assert_node(
                &node.left.clone().unwrap(),
                NodeType::Literal,
                line,
                "$2002",
            );
            assert_node(&node.right.clone().unwrap(), NodeType::Value, line, "x");
        }
    }

    #[test]
    fn indirect_addressing_bare() {
        for line in vec![
            "lda ($2000)",
            "    lda   ( $2000 )  ; Comment",
            "  : lda (   $2000)",
            "lda($2000)",
        ]
        .into_iter()
        {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Instruction, line, "lda");
            assert!(node.right.is_none());

            let left = node.left.clone().unwrap();
            assert_eq!(left.node_type, NodeType::Indirection);
            assert_node(&left.left.unwrap(), NodeType::Literal, line, "$2000");
            assert!(left.right.is_none());
        }
    }

    #[test]
    fn indirect_addressing_x() {
        for line in vec![
            "lda ($20, x)",
            "    lda ($20,   x)",
            " lda   ($20,x)  ",
            "  : lda ($20  ,   x) ; Comment",
            "  lda (  $20  ,   x  )  ",
        ]
        .into_iter()
        {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Instruction, line, "lda");
            assert!(node.right.is_none());

            let left = node.left.clone().unwrap();
            assert_eq!(left.node_type, NodeType::Indirection);
            assert_node(&left.left.unwrap(), NodeType::Literal, line, "$20");
            assert_node(&left.right.unwrap(), NodeType::Value, line, "x");
        }
    }

    #[test]
    fn bad_indirect_addressing_x() {
        let mut parser = Parser::default();

        let err = parser
            .parse("lda (Variable, x), y".as_bytes(), SourceInfo::default())
            .unwrap_err();
        assert_eq!(err.first().unwrap().message, "bad indirect addressing");
    }

    #[test]
    fn indirect_addressing_y() {
        for line in vec!["lda ($20), y"].into_iter() {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Instruction, line, "lda");

            let left = node.left.clone().unwrap();
            assert_eq!(left.node_type, NodeType::Indirection);
            assert_node(&left.left.unwrap(), NodeType::Literal, line, "$20");
            assert!(left.right.is_none());

            let right = node.right.clone().unwrap();
            assert_node(&right, NodeType::Value, line, "y");
        }
    }

    #[test]
    fn variable_in_instruction() {
        let line = "lda Variable, x";
        let mut parser = Parser::default();
        assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

        let node = parser.nodes.last().unwrap().last().unwrap();
        assert_node(node, NodeType::Instruction, line, "lda");
        assert!(node.args.is_none());

        assert_node(
            &node.left.clone().unwrap(),
            NodeType::Value,
            line,
            "Variable",
        );
        assert_node(&node.right.clone().unwrap(), NodeType::Value, line, "x");
    }

    #[test]
    fn variable_literal_in_instruction() {
        let line = "lda #Variable, x";
        let mut parser = Parser::default();
        assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

        let node = parser.nodes.last().unwrap().last().unwrap();
        assert_node(node, NodeType::Instruction, line, "lda");
        assert!(node.args.is_none());

        assert_node(
            &node.left.clone().unwrap(),
            NodeType::Literal,
            line,
            "#Variable",
        );
        assert_node(&node.right.clone().unwrap(), NodeType::Value, line, "x");
    }

    #[test]
    fn scoped_variable_literal_in_instruction() {
        for var in vec!["Scope::Variable", "Scope::Inner::Variable"].into_iter() {
            let line = format!("lda #{}", var);
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Instruction, line.as_str(), "lda");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            assert_node(
                &node.left.clone().unwrap(),
                NodeType::Literal,
                line.as_str(),
                format!("#{}", var).as_str(),
            );
        }
    }

    #[test]
    fn bad_variable_scoping() {
        let mut parser = Parser::default();

        let err = parser
            .parse("adc #One:Variable".as_bytes(), SourceInfo::default())
            .unwrap_err();
        assert_eq!(
            err.first().unwrap().message,
            "not expecting a label defined here"
        );
    }

    #[test]
    fn reserved_mnemonic_name() {
        let mut parser = Parser::default();

        let err = parser
            .parse("lda = $10".as_bytes(), SourceInfo::default())
            .unwrap_err();
        assert_eq!(
            err.first().unwrap().message,
            "cannot use the reserved mnemonic 'lda' as a variable name"
        );
    }

    #[test]
    fn relative_labels() {
        for label in vec![":+", ":++", ":+++ ", ":++++", ":-", ":--", ":---", ":----"].into_iter() {
            let line = format!("jmp {}", label);
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Instruction, line.as_str(), "jmp");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            assert_node(
                &node.left.clone().unwrap(),
                NodeType::Value,
                line.as_str(),
                label.trim(),
            );
        }
    }

    #[test]
    fn bad_relative_labels() {
        let mut parser = Parser::default();

        let mut line = "jmp :+++++";
        let mut err = parser
            .parse(line.as_bytes(), SourceInfo::default())
            .unwrap_err();
        assert_eq!(
            err.first().unwrap().message,
            "you can only jump to a maximum of four relative labels"
        );

        line = "jmp :+-";
        err = parser
            .parse(line.as_bytes(), SourceInfo::default())
            .unwrap_err();
        assert_eq!(
            err.first().unwrap().message,
            "forward relative label can only have '+' characters"
        );

        line = "jmp :-+-";
        err = parser
            .parse(line.as_bytes(), SourceInfo::default())
            .unwrap_err();
        assert_eq!(
            err.first().unwrap().message,
            "backward relative label can only have '-' characters"
        );

        line = "jmp Identifier:++";
        err = parser
            .parse(line.as_bytes(), SourceInfo::default())
            .unwrap_err();
        assert_eq!(
            err.first().unwrap().message,
            "you cannot have a relative label inside of an identifier"
        );
    }

    // Assignments

    #[test]
    fn bad_assignments() {
        let mut parser = Parser::default();

        let mut err = parser
            .parse("abc = $10".as_bytes(), SourceInfo::default())
            .unwrap_err();
        assert_eq!(
            err.first().unwrap().message,
            "cannot use names which are valid hexadecimal values such as 'abc'"
        );

        parser = Parser::default();
        err = parser
            .parse(".var = 1".as_bytes(), SourceInfo::default())
            .unwrap_err();
        assert_eq!(
            err.first().unwrap().message,
            "'.' characters are not allowed for variables"
        );

        parser = Parser::default();
        err = parser
            .parse("var =".as_bytes(), SourceInfo::default())
            .unwrap_err();
        assert_eq!(err.first().unwrap().message, "incomplete assignment");

        parser = Parser::default();
        err = parser
            .parse("var =   ".as_bytes(), SourceInfo::default())
            .unwrap_err();
        assert_eq!(err.first().unwrap().message, "incomplete assignment");

        parser = Parser::default();
        err = parser
            .parse("var =   ; Comment".as_bytes(), SourceInfo::default())
            .unwrap_err();
        assert_eq!(err.first().unwrap().message, "incomplete assignment");
    }

    // Constant expressions

    #[test]
    fn constant_expression_test() {
        let line = "ldx #(4 * NUM_SPRITES)";
        let mut parser = Parser::default();
        assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

        let node = parser.nodes.last().unwrap().last().unwrap();
        assert_node(node, NodeType::Instruction, line, "ldx");
        assert!(node.args.is_none());
        assert!(node.right.is_none());

        let literal = &node.left.clone().unwrap();
        assert_eq!(literal.node_type, NodeType::Literal);

        let op = &literal.left.clone().unwrap();
        assert_eq!(op.node_type, NodeType::Operation(OperationType::Mul));
        assert_node(&op.left.clone().unwrap(), NodeType::Value, line, "4");
        assert_node(
            &op.right.clone().unwrap(),
            NodeType::Value,
            line,
            "NUM_SPRITES",
        );
    }

    #[test]
    fn nested_expression_test() {
        let line = "lda #$80 >> ((BCD_BITS - 1) & 3)";
        let mut parser = Parser::default();
        assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

        let instr = parser.nodes.last().unwrap().last().unwrap();
        assert_node(instr, NodeType::Instruction, line, "lda");
        assert!(instr.args.is_none());
        assert!(instr.right.is_none());

        // Shift object
        let left = instr.left.as_ref().unwrap();
        assert_eq!(left.node_type, NodeType::Operation(OperationType::Rshift));

        // Left arm of the shift.
        assert_node(left.left.as_ref().unwrap(), NodeType::Literal, line, "#$80");

        // Right arm of the shift: bitwise and.
        let right = left.right.as_ref().unwrap();
        assert_eq!(right.node_type, NodeType::Operation(OperationType::And));

        // Left arm of the bitwise and.
        let left_and = right.left.as_ref().unwrap();
        assert_eq!(left_and.node_type, NodeType::Operation(OperationType::Sub));
        assert_node(
            left_and.left.as_ref().unwrap(),
            NodeType::Value,
            line,
            "BCD_BITS",
        );
        assert_node(left_and.right.as_ref().unwrap(), NodeType::Value, line, "1");

        // Right arm of the bitwise and.
        let right_and = right.right.as_ref().unwrap();
        assert_node(right_and, NodeType::Value, line, "3");
    }

    #[test]
    fn unary_operator_test() {
        let line = "ldx #<NUM_SPRITES";
        let mut parser = Parser::default();
        assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

        let node = parser.nodes.last().unwrap().last().unwrap();
        assert_node(node, NodeType::Instruction, line, "ldx");
        assert!(node.args.is_none());
        assert!(node.right.is_none());

        let literal = &node.left.clone().unwrap();
        assert_node(literal, NodeType::Literal, line, "#<NUM_SPRITES");

        let op = &literal.left.clone().unwrap();
        assert_eq!(op.node_type, NodeType::Operation(OperationType::LoByte));
        assert!(op.left.is_none());
        assert_node(
            &op.right.clone().unwrap(),
            NodeType::Value,
            line,
            "NUM_SPRITES",
        );
    }

    // Control statements.

    #[test]
    fn parse_control_no_args() {
        for line in vec![".byte", "    .byte", " label: .byte   ; Comment"].into_iter() {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Control(ControlType::Byte), line, ".byte");
            assert!(node.left.is_none());
            assert!(node.right.is_none());
            assert!(node.args.is_none());
        }
    }

    #[test]
    fn parse_control_one_arg() {
        for line in vec![
            ".hibyte $2000",
            "    .hibyte $2000",
            " label: .hibyte $2000   ; Comment",
            "  .hibyte($2000)",
            "  .hibyte (  $2000  )",
        ]
        .into_iter()
        {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(
                node,
                NodeType::Control(ControlType::Hibyte),
                line,
                ".hibyte",
            );
            assert!(node.left.is_none());
            assert!(node.right.is_none());

            let args = node.args.clone().unwrap();
            assert_eq!(args.len(), 1);
            assert_node(args.first().unwrap(), NodeType::Literal, line, "$2000");
        }
    }

    #[test]
    fn parse_control_multiple_args() {
        for line in vec![
            ".byte $10, $20",
            "    .byte $10, $20",
            " label: .byte $10, $20   ; Comment",
            "  .byte($10, $20)",
            "  .byte (  $10 , $20  )",
        ]
        .into_iter()
        {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Control(ControlType::Byte), line, ".byte");
            assert!(node.left.is_none());
            assert!(node.right.is_none());

            let args = node.args.clone().unwrap();
            assert_eq!(args.len(), 2);
            assert_node(args.first().unwrap(), NodeType::Literal, line, "$10");
            assert_node(args.last().unwrap(), NodeType::Literal, line, "$20");
        }
    }

    #[test]
    fn parse_byte_with_character_literals() {
        let line = ".byte 'N', 'E', 'S', $1A";
        let mut parser = Parser::default();
        assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

        let args = parser
            .nodes
            .last()
            .unwrap()
            .first()
            .unwrap()
            .args
            .clone()
            .unwrap();
        assert_eq!(args.len(), 4);

        let mut it = args.into_iter();

        let mut cur = it.next().unwrap();
        let mut left = cur.left.as_ref().unwrap();
        assert_node(&cur, NodeType::Literal, line, "'N'");
        assert_eq!(&left.node_type, &NodeType::Value);
        assert_eq!(&left.value.value, "78");
        assert_eq!(line.get(left.value.start..left.value.end).unwrap(), "N");

        cur = it.next().unwrap();
        left = cur.left.as_ref().unwrap();
        assert_node(&cur, NodeType::Literal, line, "'E'");
        assert_eq!(&left.node_type, &NodeType::Value);
        assert_eq!(&left.value.value, "69");
        assert_eq!(line.get(left.value.start..left.value.end).unwrap(), "E");

        cur = it.next().unwrap();
        left = cur.left.as_ref().unwrap();
        assert_node(&cur, NodeType::Literal, line, "'S'");
        assert_eq!(&left.node_type, &NodeType::Value);
        assert_eq!(&left.value.value, "83");
        assert_eq!(line.get(left.value.start..left.value.end).unwrap(), "S");
    }

    #[test]
    fn parse_control_id_no_args() {
        for line in vec![
            ".scope Scope",
            "    .scope Scope",
            " label: .scope Scope   ; Comment",
            "  .scope   Scope",
        ]
        .into_iter()
        {
            // Add `.endscope` or it will error out on an unclosed macro
            // statement.
            let real = String::from(line) + "\n.endscope";

            let mut parser = Parser::default();
            assert!(parser.parse(real.as_bytes(), SourceInfo::default()).is_ok());

            let nodes = parser.nodes();
            let node = &nodes[nodes.len() - 2];
            assert_node(
                node,
                NodeType::Control(ControlType::StartScope),
                line,
                ".scope",
            );
            assert!(node.right.is_some());
            assert!(node.args.is_none());

            let left = node.left.clone().unwrap();
            assert_node(&left, NodeType::Value, line, "Scope");
        }
    }

    #[test]
    fn parse_control_id_one_arg() {
        for line in vec![
            ".macro Macro(arg1)",
            ".macro Macro arg1 ",
            "    .macro   Macro(arg1)",
            " label: .macro Macro(arg1)   ; Comment",
            "  .macro   Macro  ( arg1 )",
        ]
        .into_iter()
        {
            // Add `.endmacro` or it will error out on an unclosed macro
            // statement.
            let real = String::from(line) + "\n.endmacro";

            let mut parser = Parser::default();
            assert!(parser.parse(real.as_bytes(), SourceInfo::default()).is_ok());

            let nodes = parser.nodes();
            let node = &nodes[nodes.len() - 2];
            assert_node(
                node,
                NodeType::Control(ControlType::StartMacro),
                line,
                ".macro",
            );
            assert!(node.right.is_some());

            let left = node.left.clone().unwrap();
            assert_node(&left, NodeType::Value, line, "Macro");

            let args = node.args.clone().unwrap();
            assert_eq!(args.len(), 1);
            assert_node(args.first().unwrap(), NodeType::Value, line, "arg1");
        }
    }

    #[test]
    fn parse_control_id_multiple_args() {
        for line in vec![
            ".macro Macro(arg1, arg2)",
            ".macro Macro arg1, arg2 ",
            "    .macro   Macro(arg1, arg2)",
            " label: .macro Macro(arg1, arg2)   ; Comment",
            "  .macro   Macro  ( arg1 , arg2 )",
        ]
        .into_iter()
        {
            // Add `.endmacro` or it will error out on an unclosed macro
            // statement.
            let real = String::from(line) + "\n.endmacro";

            let mut parser = Parser::default();
            assert!(parser.parse(real.as_bytes(), SourceInfo::default()).is_ok());

            let nodes = parser.nodes();
            let node = &nodes[nodes.len() - 2];
            assert_node(
                node,
                NodeType::Control(ControlType::StartMacro),
                line,
                ".macro",
            );
            assert!(node.right.is_some());

            let left = node.left.clone().unwrap();
            assert_node(&left, NodeType::Value, line, "Macro");

            let args = node.args.clone().unwrap();
            assert_eq!(args.len(), 2);
            assert_node(args.first().unwrap(), NodeType::Value, line, "arg1");
            assert_node(args.last().unwrap(), NodeType::Value, line, "arg2");
        }
    }

    #[test]
    fn parse_control_unclosed() {
        let mut parser = Parser::default();
        let err = parser
            .parse(".macro MACRO".as_bytes(), SourceInfo::default())
            .unwrap_err();

        assert_eq!(
            err.first().unwrap().message,
            "expecting a 'control function (.endmacro)' but there are no more statements"
        );
    }

    #[test]
    fn parse_control_too_many_closes() {
        let mut parser = Parser::default();
        let err = parser
            .parse(".endmacro".as_bytes(), SourceInfo::default())
            .unwrap_err();

        assert_eq!(
            err.first().unwrap().message,
            "unexpected 'control function (.endmacro)'"
        );
    }

    #[test]
    fn parse_control_wrong_close() {
        let code = r#".scope Scope
    .macro Macro
    .endscope
    .endmacro"#;
        let mut parser = Parser::default();
        let err = parser
            .parse(code.as_bytes(), SourceInfo::default())
            .unwrap_err();

        assert_eq!(
            err.first().unwrap().message,
            "expecting 'control function (.endmacro)', found 'control function (.endscope)'"
        );
        assert_eq!(
            err.last().unwrap().message,
            "expecting 'control function (.endscope)', found 'control function (.endmacro)'"
        );
    }

    #[test]
    fn parse_control_body() {
        let code = r#".macro MACRO
nop
inc $20
.endmacro"#;
        let mut parser = Parser::default();
        assert!(parser.parse(code.as_bytes(), SourceInfo::default()).is_ok());

        let nodes = parser.nodes();
        assert_eq!(nodes.len(), 2); // .macro and .endmacro

        let node = nodes.first().unwrap();
        assert_node(
            node,
            NodeType::Control(ControlType::StartMacro),
            code,
            ".macro",
        );

        let left = node.left.clone().unwrap();
        assert_node(&left, NodeType::Value, code, "MACRO");

        let right = node.right.clone().unwrap();
        assert_node(&right, NodeType::ControlBody, code, "");

        let inner = right.args.unwrap();
        assert_node(inner.first().unwrap(), NodeType::Instruction, "nop", "nop");
        assert_node(
            inner.last().unwrap(),
            NodeType::Instruction,
            "inc $20",
            "inc",
        );
    }

    #[test]
    fn parse_control_bad_number_args() {
        for line in vec![".hibyte", ".hibyte($20, $22)"].into_iter() {
            let mut parser = Parser::default();
            let err = parser
                .parse(line.as_bytes(), SourceInfo::default())
                .unwrap_err();

            assert_eq!(
                err.first().unwrap().message,
                "wrong number of arguments for function '.hibyte'"
            );
        }
    }

    #[test]
    fn parse_control_in_instructions() {
        for line in vec!["lda #.hibyte($2010)", "  label:   lda #.hibyte   $2010   "].into_iter() {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Instruction, line, "lda");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            let left = node.left.clone().unwrap();
            assert_node(&left, NodeType::Literal, line, "#.hibyte");
            assert!(left.right.is_none());
            assert!(left.args.is_none());

            let control = left.left.clone().unwrap();
            assert_node(
                &control,
                NodeType::Control(ControlType::Hibyte),
                line,
                ".hibyte",
            );
            assert!(control.left.is_none());
            assert!(control.right.is_none());

            let args = control.args.clone().unwrap();
            assert_eq!(args.len(), 1);
            assert_node(args.first().unwrap(), NodeType::Literal, line, "$2010");
        }
    }

    #[test]
    fn parse_control_in_indirect_x_instructions() {
        for line in vec![
            "lda (#.hibyte($2010), x)",
            " label:   lda (#.hibyte ( $2010 ) , x)",
        ]
        .into_iter()
        {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Instruction, line, "lda");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            let ind = node.left.clone().unwrap();
            assert_eq!(ind.node_type, NodeType::Indirection);
            assert!(ind.args.is_none());

            let left = ind.left.clone().unwrap();
            assert_node(&left, NodeType::Literal, line, "#.hibyte");
            assert!(left.right.is_none());
            assert!(left.args.is_none());

            let control = left.left.clone().unwrap();
            assert_node(
                &control,
                NodeType::Control(ControlType::Hibyte),
                line,
                ".hibyte",
            );
            assert!(control.left.is_none());
            assert!(control.right.is_none());

            let args = control.args.clone().unwrap();
            assert_eq!(args.len(), 1);
            assert_node(args.first().unwrap(), NodeType::Literal, line, "$2010");

            let right = ind.right.clone().unwrap();
            assert_node(&right, NodeType::Value, line, "x");
        }
    }

    #[test]
    fn parse_control_in_indirect_y_instructions() {
        for line in vec![
            "lda (#.hibyte($2010)), y",
            "  label:   lda ( #.hibyte( $2010  ) )  , y",
        ]
        .into_iter()
        {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Instruction, line, "lda");
            assert!(node.args.is_none());

            let ind = node.left.clone().unwrap();
            assert_eq!(ind.node_type, NodeType::Indirection);
            assert!(ind.right.is_none());
            assert!(ind.args.is_none());

            let left = ind.left.clone().unwrap();
            assert_node(&left, NodeType::Literal, line, "#.hibyte");
            assert!(left.right.is_none());
            assert!(left.args.is_none());

            let control = left.left.clone().unwrap();
            assert_node(
                &control,
                NodeType::Control(ControlType::Hibyte),
                line,
                ".hibyte",
            );
            assert!(control.left.is_none());
            assert!(control.right.is_none());

            let args = control.args.clone().unwrap();
            assert_eq!(args.len(), 1);
            assert_node(args.first().unwrap(), NodeType::Literal, line, "$2010");

            let right = node.right.clone().unwrap();
            assert_node(&right, NodeType::Value, line, "y");
        }
    }

    #[test]
    fn parse_control_in_assignments() {
        for line in vec![
            "lala = #.hibyte($2010)",
            "  lala   =     #.hibyte($2010)",
            "label:  lala   =     #.hibyte($2010)  ; comment",
        ]
        .into_iter()
        {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Assignment, line, "lala");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            let left = node.left.clone().unwrap();
            assert_node(&left, NodeType::Literal, line, "#.hibyte");
            assert!(left.right.is_none());
            assert!(left.args.is_none());

            let control = left.left.clone().unwrap();
            assert_node(
                &control,
                NodeType::Control(ControlType::Hibyte),
                line,
                ".hibyte",
            );
            assert!(control.left.is_none());
            assert!(control.right.is_none());

            let args = control.args.clone().unwrap();
            assert_eq!(args.len(), 1);
            assert_node(args.first().unwrap(), NodeType::Literal, line, "$2010");
        }
    }

    #[test]
    fn parse_repeat_control() {
        let mut parser = Parser::default();
        let err = parser
            .parse(".repeat\n.endrepeat".as_bytes(), SourceInfo::default())
            .unwrap_err();
        assert_eq!(
            err.first().unwrap().message,
            "wrong number of arguments for function '.repeat'"
        );

        let mut parser = Parser::default();
        let err = parser
            .parse(
                ".repeat 1, 2, 3\n.endrepeat".as_bytes(),
                SourceInfo::default(),
            )
            .unwrap_err();
        assert_eq!(
            err.first().unwrap().message,
            "wrong number of arguments for function '.repeat'"
        );

        // Minimum required argument.

        parser = Parser::default();
        let mut line = ".repeat 2\n.endrepeat";

        assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());
        let mut control = parser.nodes.last().unwrap().first().unwrap();
        assert_node(
            control,
            NodeType::Control(ControlType::StartRepeat),
            line,
            ".repeat",
        );
        assert!(control
            .left
            .as_ref()
            .unwrap()
            .value
            .value
            .starts_with(".repeat-"));
        assert!(control.right.is_some());

        let mut args = control.args.clone().unwrap();
        assert_eq!(args.len(), 1);
        assert_node(args.first().unwrap(), NodeType::Value, line, "2");

        // Maximum allowed arguments.

        parser = Parser::default();
        line = ".repeat 2, I\n.endrepeat";

        assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());
        control = parser.nodes.last().unwrap().first().unwrap();
        assert_node(
            control,
            NodeType::Control(ControlType::StartRepeat),
            line,
            ".repeat",
        );
        assert!(control
            .left
            .as_ref()
            .unwrap()
            .value
            .value
            .starts_with(".repeat-"));
        assert!(control.right.is_some());

        args = control.args.clone().unwrap();
        assert_eq!(args.len(), 2);
        assert_node(args.first().unwrap(), NodeType::Value, line, "2");
        assert_node(args.last().unwrap(), NodeType::Value, line, "I");
    }

    #[test]
    fn parse_unknown_control() {
        let mut parser = Parser::default();
        let err = parser
            .parse(".".as_bytes(), SourceInfo::default())
            .unwrap_err();
        assert_eq!(err.first().unwrap().message, "empty identifier");

        parser = Parser::default();
        assert!(parser
            .parse(".whatever".as_bytes(), SourceInfo::default())
            .is_ok());
    }

    // Macro calls.

    #[test]
    fn parse_macro_call_no_args_variable_lookalike() {
        for line in vec![
            "MACRO_CALL",
            " MACRO_CALL ",
            " label:  MACRO_CALL  ; comment",
        ]
        .into_iter()
        {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Call, line, "MACRO_CALL");
            assert!(node.left.is_none());
            assert!(node.right.is_none());
            assert!(node.args.is_none());
        }
    }

    #[test]
    fn parse_macro_call_no_args() {
        for line in vec![
            "MACRO_CALL()",
            " MACRO_CALL() ",
            " MACRO_CALL () ",
            " MACRO_CALL (  ) ",
            " label:  MACRO_CALL ()  ; comment",
        ]
        .into_iter()
        {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Call, line, "MACRO_CALL");
            assert!(node.left.is_none());
            assert!(node.right.is_none());
            assert!(node.args.is_none());
        }
    }

    #[test]
    fn parse_macro_call_one_arg() {
        for line in vec![
            "MACRO_CALL(arg1)",
            "MACRO_CALL arg1 ",
            "    MACRO_CALL (arg1)",
            " label: MACRO_CALL( arg1 )   ; Comment",
        ]
        .into_iter()
        {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Call, line, "MACRO_CALL");
            assert!(node.left.is_none());
            assert!(node.right.is_none());

            let args = node.args.clone().unwrap();
            assert_eq!(args.len(), 1);
            assert_node(args.first().unwrap(), NodeType::Value, line, "arg1");
        }
    }

    #[test]
    fn parse_macro_call_multiple_args() {
        for line in vec![
            "MACRO_CALL(arg1, arg2)",
            "MACRO_CALL arg1, arg2 ",
            "    MACRO_CALL (arg1,arg2)",
            " label: MACRO_CALL( arg1 , arg2 )   ; Comment",
        ]
        .into_iter()
        {
            let mut parser = Parser::default();
            assert!(parser.parse(line.as_bytes(), SourceInfo::default()).is_ok());

            let node = parser.nodes.last().unwrap().last().unwrap();
            assert_node(node, NodeType::Call, line, "MACRO_CALL");
            assert!(node.left.is_none());
            assert!(node.right.is_none());

            let args = node.args.clone().unwrap();
            assert_eq!(args.len(), 2);
            assert_node(args.first().unwrap(), NodeType::Value, line, "arg1");
            assert_node(args.last().unwrap(), NodeType::Value, line, "arg2");
        }
    }
}
