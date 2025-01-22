use crate::mapping::{Mapping, SectionType, Segment};
use std::collections::HashMap;

// Section of the configuration being parsed.
enum Section {
    Memory,
    Segment,
    Symbol,
}

// Same as mapping::Mapping but before translating symbols into it. Used when
// fetching the values from the configuration file but before symbols have been
// all loaded.
struct RawMapping {
    name: String,
    start: String,
    size: String,
    fill: Option<String>,
    ignore: bool,
    line_num: usize,
    segments: String,
}

// Fetch a line of values in the format of "Name: key1=value1, key2=value2,
// ...;". The semicolon at the end is not considered and the given line should
// not contain it as the caller must have check it beforehand. Returns a tuple
// which contain the name (i.e. what's before the colon) and the line of values
// (i.e. after the colon and before the semicolon).
fn fetch_line_values(line: &str, line_num: usize) -> Result<(String, String), String> {
    let mut name_values = line.split(':');
    let Some(name) = name_values.next() else {
        return Err(format!(
            "line does not follow 'key: values' format (line {})",
            line_num
        ));
    };
    let Some(values_line) = name_values.next() else {
        return Err(format!(
            "line does not follow 'key: values' format (line {})",
            line_num
        ));
    };
    if name_values.next().is_some() {
        return Err(format!(
            "line does not follow 'key: values' format (line {})",
            line_num
        ));
    }

    Ok((name.trim().to_string(), values_line.trim().to_string()))
}

// Returns the value contained in the given `string` as an hexadecimal number.
// If it doesn't start with '$', then it's assumed to be a symbol reference that
// can be found in `symbols`.
fn get_hex_from(
    string: &String,
    line_num: usize,
    symbols: &HashMap<String, usize>,
) -> Result<usize, String> {
    if string.is_empty() {
        return Ok(0);
    }

    if !string.starts_with('$') {
        match symbols.get(string) {
            Some(value) => return Ok(*value),
            None => return Err(format!("malformed hex value (line {})", line_num)),
        }
    }

    let val = &string[1..string.len()];
    if val.is_empty() || val.len() > 4 {
        return Err(format!("malformed hex value (line {})", line_num));
    }

    Ok(usize::from_str_radix(val, 16).unwrap())
}

// Returns the RawMapping that can be extracted by treating the given `line` as
// a line from the "Memory" section.
fn fetch_memory_definition(line: &str, line_num: usize) -> Result<RawMapping, String> {
    let (name, values) = fetch_line_values(line, line_num)?;
    let mut res = RawMapping {
        name,
        start: String::from(""),
        size: String::from(""),
        fill: None,
        ignore: false,
        line_num,
        segments: String::from(""),
    };

    for value in values.split(',') {
        let mut key_value = value.trim().split('=');

        match key_value.next() {
            Some(key) => {
                let val = key_value.next().unwrap_or("").trim();

                match key.trim() {
                    "file" => res.ignore = val != "%O",
                    "fill" => {
                        if res.fill.is_none() {
                            res.fill = Some(String::from(""));
                        }
                    }
                    "fillval" => res.fill = Some(val.to_string()),
                    "start" => res.start = val.to_string(),
                    "size" => res.size = val.to_string(),
                    "segments" => res.segments = val.to_string(),
                    _ => {}
                }
            }
            None => return Err(format!("malformed key-value (line {})", line_num)),
        }
    }

    Ok(res)
}

// Returns a tuple with the name of the segment and the memory section being
// referenced in this segment.
fn fetch_segment_definition(line: &str, line_num: usize) -> Result<(String, String), String> {
    let (name, values) = fetch_line_values(line, line_num)?;

    if find_value("start", &values, line_num).is_ok() {
        return Err("setting a 'start' at the segment level is not supported".to_string());
    }
    Ok((name, find_value("load", &values, line_num)?))
}

// Returns the value for the referenced `key` which is inside of the string of
// `values`.
fn find_value(key: &str, values: &str, line_num: usize) -> Result<String, String> {
    for value in values.split(',') {
        let mut key_value = value.trim().split('=');

        match key_value.next() {
            Some(k) => {
                if k.trim() == key {
                    let val = key_value.next().unwrap_or("").trim();
                    return Ok(val.to_string());
                }
            }
            None => return Err(format!("malformed key-value (line {})", line_num)),
        }
    }

    Err(format!(
        "could not find '{}' definition (line {})",
        key, line_num
    ))
}

/// Parse the given blob of `text` as a .cfg file as it's expected by ld65:
/// https://www.cc65.org/doc/ld65-5.html.
pub fn parse_cfg_file(text: &str) -> Result<Vec<Mapping>, String> {
    let mut values = vec![];
    let mut raw_segments = vec![];
    let mut should_skip = false;
    let mut section = None;
    let mut symbols = HashMap::new();

    // 1. Get the raw data and build the `symbols` hash.
    //
    // This is done by considering only three sections: symbols, memory, and segments.
    for (idx, line) in text.lines().enumerate() {
        // For this given line, maybe it's empty or it's a comment. Another case
        // is that it's a section we just don't care. In either case, just skip
        // to the next line.
        let l = line.trim();
        if should_skip {
            if l == "}" {
                // End of section, we might care about the next line.
                should_skip = false;
            }
            continue;
        }
        if l.is_empty() || l.starts_with('#') {
            continue;
        }

        // If we are inside of a section, parse it.
        if section.is_some() {
            if l == "}" {
                should_skip = false;
                section = None;
                continue;
            }

            // Parse up until the closing semicolon, as passed it there might be
            // an inline comment.
            let tline = match l.find(';') {
                Some(idx) => &l[0..idx],
                None => {
                    return Err(format!(
                        "line does not end with a semicolon (line {})",
                        idx + 1
                    ))
                }
            };

            // For each section there is a different action for the current
            // line. Handle this now.
            match section.as_ref().unwrap() {
                Section::Memory => {
                    let definition = fetch_memory_definition(tline, idx + 1)?;
                    if !definition.ignore {
                        values.push(definition);
                    }
                }
                Section::Segment => raw_segments.push(fetch_segment_definition(tline, idx + 1)?),
                Section::Symbol => {
                    let (name, values) = fetch_line_values(tline, idx + 1)?;
                    let value = find_value("value", &values, idx + 1)?;
                    symbols.insert(name, get_hex_from(&value, idx + 1, &symbols)?);
                }
            }

            continue;
        }

        // We are not in a section and the line is not empty. Thus, this has to
        // be a section definition. Parse the name and check if we actually have
        // to care about it or not.
        match l.find('{') {
            Some(idx) => {
                let id = l[0..idx].trim().to_lowercase();
                match id.as_str() {
                    "memory" => section = Some(Section::Memory),
                    "segments" => section = Some(Section::Segment),
                    "symbols" => section = Some(Section::Symbol),
                    _ => should_skip = true,
                }
            }
            None => {
                return Err(format!(
                    "section must end with an opening bracket (line {})",
                    idx + 1
                ))
            }
        };
    }

    let mut header = true;
    let mut res = vec![];
    let mut section_type = SectionType::Header;

    // 2. Parse each data point so we can build a Mapping out of it.
    for mapping in &values {
        // We can now figure out the `start`, the `size` and the `fill`
        // properties given that the `symbols` should all be known at this
        // point.
        let start = get_hex_from(&mapping.start, mapping.line_num, &symbols)? as u16;
        let size = get_hex_from(&mapping.size, mapping.line_num, &symbols)?;
        let fill = match &mapping.fill {
            Some(f) => Some(get_hex_from(f, mapping.line_num, &symbols)? as u8),
            None => None,
        };

        // The section type is a bit tricky because that's not something
        // considered on ld65. Hence, we make these assumptions:
        //   1. If it's the first mapping we see, then it's the header.
        //   2. If after the header it claims to start at 0x00, then from now on
        //      it's CHR ROM.
        //   3. Otherwise it's PRG ROM unless in a previous iteration we
        //      realized it's CHR ROM.
        if header {
            header = false;
        } else if start == 0x0000 {
            section_type = SectionType::ChrRom;
        } else if section_type != SectionType::ChrRom {
            section_type = SectionType::PrgRom;
        };

        // Push in order the segments which are to be loaded on the current
        // mapping.
        let mut segments = vec![];
        for (segment_name, load) in &raw_segments {
            if mapping.name == *load {
                segments.push(Segment::from(segment_name.as_str()));
            }
        }

        res.push(Mapping {
            name: mapping.name.clone(),
            start,
            size,
            offset: 0,
            fill,
            segments,
            section_type: section_type.clone(),
        })
    }

    Ok(res)
}

fn get_segments_from(value: &str, line: usize) -> Result<Vec<Segment>, String> {
    if !value.starts_with('[') || !value.ends_with(']') {
        return Err(format!("should be enclosed inside of [] (line {})", line));
    }

    let mut res = vec![];
    for name in value[1..value.len() - 1].split(' ') {
        let trimmed_name = name.trim();
        res.push(Segment::from(trimmed_name));
    }

    Ok(res)
}

/// Parse the given blob of `text` as a simplified .cfg file.
pub fn parse_nasm_cfg_file(text: &str) -> Result<Vec<Mapping>, String> {
    let mut mappings = vec![];
    let symbols = HashMap::new();
    let mut header = true;
    let mut section_type = SectionType::Header;

    for (idx, line) in text.lines().enumerate() {
        // Skip empty lines and comments.
        let l = line.trim();
        if l.is_empty() || l.starts_with('#') {
            continue;
        }

        let real_line = match l.find(';') {
            Some(idx) => &l[0..idx],
            None => {
                return Err(format!(
                    "line does not end with a semicolon (line {})",
                    idx + 1
                ))
            }
        };

        // Parse the mapping definition.
        let mapping = fetch_memory_definition(real_line, idx + 1)?;

        // Parse hexadecimal values from start, size and fill.
        let start = get_hex_from(&mapping.start, mapping.line_num, &symbols)? as u16;
        let size = get_hex_from(&mapping.size, mapping.line_num, &symbols)?;
        let fill = match &mapping.fill {
            Some(f) => Some(get_hex_from(f, mapping.line_num, &symbols)? as u8),
            None => None,
        };

        // As with 'parse_cfg_file', the section type:
        //   1. If it's the first mapping we see, then it's the header.
        //   2. If after the header it claims to start at 0x00, then from now on
        //      it's CHR ROM.
        //   3. Otherwise it's PRG ROM unless in a previous iteration we
        //      realized it's CHR ROM.
        if header {
            header = false;
        } else if start == 0x0000 {
            section_type = SectionType::ChrRom;
        } else if section_type != SectionType::ChrRom {
            section_type = SectionType::PrgRom;
        };

        mappings.push(Mapping {
            name: mapping.name,
            offset: 0,
            start,
            size,
            fill,
            section_type: section_type.clone(),
            segments: get_segments_from(&mapping.segments, idx + 1)?,
        });
    }

    Ok(mappings)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_default_cc65_configuration() {
        let res = parse_cfg_file(
            r#"
SYMBOLS {
    __STACKSIZE__: type = weak, value = $0300; # 3 pages stack
}
MEMORY {
    ZP:     file = "", start = $0002, size = $001A, type = rw, define = yes;

    # INES Cartridge Header
    HEADER: file = %O, start = $0000, size = $0010, fill = yes;

    # 2 16K ROM Banks
    # - startup
    # - code
    # - rodata
    # - data (load)
    ROM0:   file = %O, start = $8000, size = $7FFA, fill = yes, define = yes;

    # Hardware Vectors at End of 2nd 8K ROM
    ROMV:   file = %O, start = $FFFA, size = $0006, fill = yes;

    # 1 8k CHR Bank
    ROM2:   file = %O, start = $0000, size = $2000, fill = yes;

    # standard 2k SRAM (-zeropage)
    # $0100-$0200 cpu stack
    # $0200-$0500 3 pages for ppu memory write buffer
    # $0500-$0800 3 pages for cc65 parameter stack
    SRAM:   file = "", start = $0500, size = __STACKSIZE__, define = yes;

    # additional 8K SRAM Bank
    # - data (run)
    # - bss
    # - heap
    RAM:    file = "", start = $6000, size = $2000, define = yes;
}
SEGMENTS {
    ZEROPAGE: load = ZP,              type = zp;
    HEADER:   load = HEADER,          type = ro;
    STARTUP:  load = ROM0,            type = ro,  define   = yes;
    LOWCODE:  load = ROM0,            type = ro,  optional = yes;
    ONCE:     load = ROM0,            type = ro,  optional = yes;
    CODE:     load = ROM0,            type = ro,  define   = yes;
    RODATA:   load = ROM0,            type = ro,  define   = yes;
    DATA:     load = ROM0, run = RAM, type = rw,  define   = yes;
    VECTORS:  load = ROMV,            type = rw;
    CHARS:    load = ROM2,            type = rw;
    BSS:      load = RAM,             type = bss, define   = yes;
}
FEATURES {
    CONDES: type    = constructor,
            label   = __CONSTRUCTOR_TABLE__,
            count   = __CONSTRUCTOR_COUNT__,
            segment = ONCE;
    CONDES: type    = destructor,
            label   = __DESTRUCTOR_TABLE__,
            count   = __DESTRUCTOR_COUNT__,
            segment = RODATA;
    CONDES: type    = interruptor,
            label   = __INTERRUPTOR_TABLE__,
            count   = __INTERRUPTOR_COUNT__,
            segment = RODATA,
            import  = __CALLIRQ__;
}
"#,
        )
        .unwrap();

        assert_eq!(res.len(), 4);

        let header = &res[0];
        assert_eq!(header.name, "HEADER");
        assert_eq!(header.start, 0x00);
        assert_eq!(header.size, 0x10);
        assert_eq!(header.fill, Some(0x00));
        assert_eq!(header.section_type, SectionType::Header);
        assert_eq!(
            header
                .segments
                .iter()
                .map(|x| x.name.clone())
                .collect::<Vec<_>>(),
            &["HEADER"]
        );

        let rom0 = &res[1];
        assert_eq!(rom0.name, "ROM0");
        assert_eq!(rom0.start, 0x8000);
        assert_eq!(rom0.size, 0x7FFA);
        assert_eq!(rom0.fill, Some(0x00));
        assert_eq!(rom0.section_type, SectionType::PrgRom);
        assert_eq!(
            rom0.segments
                .iter()
                .map(|x| x.name.clone())
                .collect::<Vec<_>>(),
            &["STARTUP", "LOWCODE", "ONCE", "CODE", "RODATA", "DATA"]
        );

        let romv = &res[2];
        assert_eq!(romv.name, "ROMV");
        assert_eq!(romv.start, 0xFFFA);
        assert_eq!(romv.size, 0x06);
        assert_eq!(romv.fill, Some(0x00));
        assert_eq!(romv.section_type, SectionType::PrgRom);
        assert_eq!(
            romv.segments
                .iter()
                .map(|x| x.name.clone())
                .collect::<Vec<_>>(),
            &["VECTORS"]
        );

        let rom2 = &res[3];
        assert_eq!(rom2.name, "ROM2");
        assert_eq!(rom2.start, 0x0000);
        assert_eq!(rom2.size, 0x2000);
        assert_eq!(rom2.fill, Some(0x00));
        assert_eq!(rom2.section_type, SectionType::ChrRom);
        assert_eq!(
            rom2.segments
                .iter()
                .map(|x| x.name.clone())
                .collect::<Vec<_>>(),
            &["CHARS"]
        );
    }

    #[test]
    fn parse_unrom_configuration() {
        // Coming from https://github.com/mssola/code.nes.
        let res = parse_cfg_file(
            r#"
##
# This is a very minimalistic linker configuration for UNROM chips. This
# configuration assumes the standard UNROM configuration, with two regions
# defined that split $8000-$FFFF, where the first half is swappable. For the
# bank switching there are 7 banks, which coupled with the fixed region it sums
# up 128KB of total PRG-ROM.
#
# NOTE: it assumes that only assembly is being used, and thus a lot of magic
# required for C programs is missing.
# NOTE: it is following the example of games such as Castlevania or Megaman.
# Thus, there is no CHR segment because it's all done through RAM.

MEMORY {
    # iNES header.
    HEADER: start = $0, size = $10, fill = yes;

    # Program RAM. Available if a battery-backed RAM was requested.
    WRAM: file = "" start = $6000, size = $2000, define = yes;

    # Swappable ROM addresses. Note the filled values. This is done so it's also
    # clear by inspecting the memory which bank we are on. This can come in
    # handy when inspecting things with an hex editor.
    PRG0: start = $8000, size = $4000, fill = yes, fillval = $f8, define = yes;
    PRG1: start = $8000, size = $4000, fill = yes, fillval = $f9, define = yes;
    PRG2: start = $8000, size = $4000, fill = yes, fillval = $fa, define = yes;
    PRG3: start = $8000, size = $4000, fill = yes, fillval = $fb, define = yes;
    PRG4: start = $8000, size = $4000, fill = yes, fillval = $fc, define = yes;
    PRG5: start = $8000, size = $4000, fill = yes, fillval = $fd, define = yes;
    PRG6: start = $8000, size = $4000, fill = yes, fillval = $fe, define = yes;

    # Fixed ROM address.
    PRG: start = $C000, size = $4000, fill = yes, fillval = $ff, define = yes;
}

SEGMENTS {
    # iNES header.
    HEADER: load = HEADER, type = ro;

    # This is the fixed bank, that spans $C000-$FFFF. Make sure to put the reset
    # code and basic stuff that you don't want ever to be gone here. This will
    # include stuff like the reset code, bank switching utilities, etc.
    FIXED: load = PRG, type = ro, define = yes;

    ##
    # Swappable banks.

    BANK0: load = PRG0, type = ro, define = yes;
    BANK1: load = PRG1, type = ro, define = yes;
    BANK2: load = PRG2, type = ro, define = yes;
    BANK3: load = PRG3, type = ro, define = yes;
    BANK4: load = PRG4, type = ro, define = yes;
    BANK5: load = PRG5, type = ro, define = yes;
    BANK6: load = PRG6, type = ro, define = yes;

    # Last but not least, the vectors must be the last thing and they are
    # expecting a very special place on the fixed bank.
    VECTORS: load = PRG, type = ro;
}
"#,
        )
        .unwrap();

        assert_eq!(res.len(), 9);

        let header = &res[0];
        assert_eq!(header.name, "HEADER");
        assert_eq!(header.start, 0x00);
        assert_eq!(header.size, 0x10);
        assert_eq!(header.fill, Some(0x00));
        assert_eq!(header.section_type, SectionType::Header);
        assert_eq!(
            header
                .segments
                .iter()
                .map(|x| x.name.clone())
                .collect::<Vec<_>>(),
            &["HEADER"]
        );

        for i in 1..=7 {
            let prg = &res[i];

            assert_eq!(prg.name, format!("PRG{}", i - 1));
            assert_eq!(prg.start, 0x8000);
            assert_eq!(prg.size, 0x4000);
            assert_eq!(prg.fill, Some(0xf8 + i as u8 - 1));
            assert_eq!(prg.section_type, SectionType::PrgRom);
            assert_eq!(
                prg.segments
                    .iter()
                    .map(|x| x.name.clone())
                    .collect::<Vec<_>>(),
                &[format!("BANK{}", i - 1)]
            );
        }

        let prg = &res[8];
        assert_eq!(prg.name, "PRG");
        assert_eq!(prg.start, 0xC000);
        assert_eq!(prg.size, 0x4000);
        assert_eq!(prg.fill, Some(0xFF));
        assert_eq!(prg.section_type, SectionType::PrgRom);
        assert_eq!(
            prg.segments
                .iter()
                .map(|x| x.name.clone())
                .collect::<Vec<_>>(),
            &["FIXED", "VECTORS"]
        );
    }
}
