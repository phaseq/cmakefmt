use lazy_static::lazy_static;
use nom::branch::alt;
use nom::bytes::complete::{escaped, tag, take_till, take_till1, take_while1};
use nom::character::complete::none_of;
use nom::sequence::delimited;
use nom::IResult;
use std::collections::HashMap;

const SCOPE_BEGIN: &'static [&'static str] =
    &["if", "else", "while", "foreach", "function", "macro"];
const SCOPE_END: &'static [&'static str] = &[
    "else",
    "endif",
    "endwhile",
    "endforeach",
    "endfunction",
    "endmacro",
];

lazy_static! {
    static ref SCOPE_ARGS_BEGIN: HashMap<&'static str, Vec<String>> = [
        ("add_library", vec!["PUBLIC", "PRIVATE", "INTERFACE"]),
        ("add_executable", vec!["PUBLIC", "PRIVATE", "INTERFACE"]),
        (
            "install",
            vec![
                // installing targets
                "TARGETS",
                "ARCHIVE",
                "LIBRARY",
                "RUNTIME",
                "OBJECTS",
                "FRAMEWORK",
                "BUNDLE",
                "PRIVATE_HEADER",
                "PUBLIC_HEADER",
                "RESOURCE",
                "PERMISSIONS",
                "CONFIGURATIONS",
                "NAMELINK_COMPONENT",

                // installing files
                "FILES",
                "PROGRAMS",
                // "PERMISSIONS",
                // "CONFIGURATIONS",
                "PATTERN",
                "REGEX",

                // installing directories
                "DIRECTORY",
                "FILE_PERMISSIONS",
                "DIRECTORY_PERMISSIONS",
                // "CONFIGURATIONS",

                // installing exports
                // "PERMISSIONS",
                // "CONFIGURATIONS",
            ]
        )
    ]
    .iter()
    .map(|(k, v)| (k.clone(), v.iter().map(|v| v.to_string()).collect()))
    .collect();
    static ref SCOPE_ARGS_END: HashMap<&'static str, Vec<String>> = [
        ("get_property", vec!["PROPERTY"]),
        (
            "install",
            vec![
                // installing targets

                // installing files

                // installing directories
                "TYPE",
                "DESTINATION",

                // installing exports
            ]
        )
    ]
    .iter()
    .map(|(k, v)| (k.clone(), v.iter().map(|v| v.to_string()).collect()))
    .collect();
    static ref INLINE_ARGS: HashMap<&'static str, Vec<String>> = [
        ("get_property", vec!["DIRECTORY", "TARGET", "SOURCE", "INSTALL", "TEST", "CACHE", "PROPERTY"]),
        (
            "install",
            vec![
                // installing targets
                "DESTINATION",
                "INCLUDES",
                "COMPONENT",

                // installing files
                "TYPE",
                // "DESTINATION",
                // "COMPONENT",
                "RENAME",

                // installing directories
                // "TYPE",
                // "DESTINATION",
                // "COMPONENT",
                "PATTERN",
                "REGEX",

                // custom install logic
                "SCRIPT",
                "CODE",
                // "COMPONENT",

                // installing exports
                "EXPORT",
                // "DESTINATION",
                "NAMESPACE",
                "FILE",
                // "COMPONENT",
            ]
        )
    ]
    .iter()
    .map(|(k, v)| (k.clone(), v.iter().map(|v| v.to_string()).collect()))
    .collect();
}

fn main() -> std::io::Result<()> {
    //let input = std::fs::read_to_string("/home/fabianb/Dev/Moduleworks/dev/CMakeLists.txt")?;
    let input =
        std::fs::read_to_string("/home/fabianb/Dev/Moduleworks/dev/mwPackages/releasecut.cmake")?;
    let input = input.as_str();
    //let mut input = "# this is a comment\ncommand(a b c \"(l\\\"ol)\" 3.13)";
    /*let input = r#"
    if (a AND (b OR c))
    add_library(abc
        PUBLIC
            # leading comment
            "path/to/.cpp" # trailing comment
    )
    endif()"#;*/

    let result = format_str(input);
    println!("{}", result);

    Ok(())
}

pub fn format_str(mut input: &str) -> String {
    let mut result = String::new();
    let mut indent_n = 0;
    while !input.is_empty() {
        if let Ok((remainder, comment)) = parse_comment(input) {
            result += &"\t".repeat(indent_n);
            result += comment;
            result.push('\n');
            input = remainder;
        } else if let Ok((remainder, function)) = parse_function(input) {
            if SCOPE_END.contains(&function.name) {
                indent_n -= 1;
            }
            result += &format_function(&function, indent_n);
            result.push('\n');
            input = remainder;
            if SCOPE_BEGIN.contains(&function.name) {
                indent_n += 1;
            }
        } else if let Ok((remainder, s)) = parse_space(input) {
            // preserve empty lines
            let num_newlines = s.chars().filter(|c| *c == '\n').count();
            if num_newlines > 1 {
                result.push_str(&"\n".repeat(num_newlines - 1));
            }
            input = remainder;
        } else {
            println!("result so far: {:?}", result);
            panic!("remainder: {:?}", input);
        }
    }
    result
}

fn format_function(function: &Function, indent_n: usize) -> String {
    let len = function_args_len(&function.args).map(|l| l + function.name.len() + 2);
    let n_indent = match len {
        Some(len) => {
            if len < 80 - 4 * indent_n {
                if SCOPE_ARGS_BEGIN.get(&function.name).is_none() {
                    None
                } else {
                    Some(indent_n + 1)
                }
            } else {
                Some(indent_n + 1)
            }
        }
        None => Some(indent_n + 1),
    };
    let args = format_function_args(&function.args, n_indent, &function.name);

    let indent = "\t".repeat(indent_n);
    if n_indent.is_some() {
        format!("{}{}({}\n{})", indent, function.name, args, indent)
    } else {
        format!("{}{}({})", indent, function.name, args)
    }
}

fn format_function_args(args: &[Arg], indent_n: Option<usize>, function_name: &str) -> String {
    let mut result = String::new();
    if args.len() == 0 {
        return result;
    }
    match indent_n {
        Some(indent_n) => {
            let mut indent = "\n".to_string() + &"\t".repeat(indent_n);
            let mut last_was_inline = false;
            let section_begin = SCOPE_ARGS_BEGIN.get(function_name);
            let section_end = match args[0] {
                Arg::Plain(v) if function_name == "install" && (v == "TARGETS") => None,
                _ => SCOPE_ARGS_END.get(function_name),
            };
            let inline = INLINE_ARGS.get(function_name);
            for arg in args {
                let this_indent = if last_was_inline { " " } else { &indent };
                last_was_inline = false;
                match arg {
                    Arg::Plain(v) => {
                        if inline.and_then(|s| s.iter().find(|s| s == v)).is_some() {
                            last_was_inline = true;
                        }

                        if section_begin
                            .and_then(|s| s.iter().find(|s| s == v))
                            .is_some()
                        {
                            indent = "\n".to_string() + &"\t".repeat(indent_n + 1);
                            result.push_str(&format!("\n{}{}", "\t".repeat(indent_n), v))
                        } else if section_end
                            .and_then(|s| s.iter().find(|s| s == v))
                            .is_some()
                        {
                            indent = "\n".to_string() + &"\t".repeat(indent_n);
                            result.push_str(&format!("\n{}{}", "\t".repeat(indent_n), v))
                        } else {
                            result.push_str(&format!("{}{}", this_indent, v))
                        };
                    }
                    Arg::Quoted(v) => result.push_str(&format!("{}\"{}\"", this_indent, v)),
                    Arg::Comment(v) => result.push_str(&format!("{}{}", this_indent, v)),
                    Arg::TrailingComment(v) => result.push_str(&format!("  {}", v)),
                    Arg::Braced(v) => result.push_str(&format!(
                        "{}({}{})",
                        this_indent,
                        format_function_args(v, Some(indent_n + 1), function_name),
                        this_indent
                    )),
                }
            }
        }
        None => {
            for arg in args {
                if !result.is_empty() {
                    result.push(' ');
                }
                match arg {
                    Arg::Plain(v) => result.push_str(&format!("{}", v)),
                    Arg::Quoted(v) => result.push_str(&format!("\"{}\"", v)),
                    Arg::Comment(_) => unreachable!(),
                    Arg::TrailingComment(_) => unreachable!(),
                    Arg::Braced(v) => result.push_str(&format!(
                        "({})",
                        format_function_args(v, None, function_name),
                    )),
                }
            }
        }
    };
    result
}

fn function_args_len(args: &[Arg]) -> Option<usize> {
    if args.is_empty() {
        return Some(0);
    }
    let mut sum = 0usize;
    for arg in args {
        match arg {
            Arg::Plain(v) => sum += v.len(),
            Arg::Quoted(v) => sum += v.len() + 2,
            Arg::Comment(_) => return None,
            Arg::TrailingComment(_) => return None,
            Arg::Braced(v) => sum += function_args_len(v)? + 2,
        }
    }
    sum += args.len() - 1;
    Some(sum)
}

/*fn format_token(input: &str) -> String {
    let needs_escape = if let Ok((input, _)) = parse_token(input) {
        if input.is_empty() {
            false
        } else {
            true
        }
    } else {
        true
    };
    if needs_escape {
        format!("\"{}\"", input)
    } else {
        input.into()
    }
}*/

fn parse_comment(input: &str) -> IResult<&str, &str> {
    tag("#")(input)?;
    take_till(|c| c == '\r' || c == '\n')(input)
}

#[derive(Debug)]
struct Function<'a> {
    name: &'a str,
    args: Vec<Arg<'a>>,
}
#[derive(Debug)]
enum Arg<'a> {
    Plain(&'a str),
    Quoted(&'a str),
    Comment(&'a str),
    TrailingComment(&'a str),
    Braced(Vec<Arg<'a>>),
}
fn parse_function<'a>(input: &'a str) -> IResult<&'a str, Function<'a>> {
    let (input, name) = parse_token(input)?;
    let (input, _) = nom::multi::many0_count(parse_space)(input)?;
    let (input, args) = delimited(tag("("), parse_function_args, tag(")"))(input)?;
    Ok((input, Function { name, args }))
}

fn parse_function_args<'a>(input: &'a str) -> IResult<&'a str, Vec<Arg<'a>>> {
    let mut r = vec![];
    let mut input = input;
    loop {
        if let Ok((i, _)) = parse_space(input) {
            input = i; // skip spaces
        }
        if parse_closing_brace(input).is_ok() {
            break;
        } else if parse_opening_brace(input).is_ok() {
            let (i, a) = delimited(tag("("), parse_function_args, tag(")"))(input)?;
            input = i;
            r.push(Arg::Braced(a));
            continue;
        } else if let Ok((i, s)) = parse_string(input) {
            input = i;
            r.push(Arg::Quoted(s));
        } else if let Ok((i, c)) = parse_comment(input) {
            input = i;
            r.push(Arg::Comment(c));
        } else if let Ok((i, t)) = parse_token(input) {
            input = i;
            r.push(Arg::Plain(t));
        }
        if let Ok((i, _)) = parse_trailing_spaces(input) {
            input = i; // skip spaces
        }
        if let Ok((i, comment)) = parse_comment(input) {
            input = i;
            r.push(Arg::TrailingComment(comment));
        }
    }
    Ok((input, r))
}

fn parse_opening_brace(input: &str) -> IResult<&str, &str> {
    tag("(")(input)
}

fn parse_closing_brace(input: &str) -> IResult<&str, &str> {
    tag(")")(input)
}

fn parse_space(input: &str) -> IResult<&str, &str> {
    take_while1(|c| c == ' ' || c == '\t' || c == '\r' || c == '\n')(input)
}

fn parse_trailing_spaces(input: &str) -> IResult<&str, &str> {
    take_while1(|c| c == ' ' || c == '\t')(input)
}

fn parse_token(input: &str) -> IResult<&str, &str> {
    //take_while1(|c| nom::AsChar::is_alphanum(c) || "_.${}-<>=/\\".contains(c))(input)
    take_till1(|c| " \t\n\r()".contains(c))(input)
}

fn parse_string(input: &str) -> IResult<&str, &str> {
    let esc = escaped(none_of("\\\""), '\\', tag("\""));
    let esc_or_empty = alt((esc, tag("")));
    delimited(tag("\""), esc_or_empty, tag("\""))(input)
}

#[cfg(test)]
mod tests {
    use crate::format_str;

    #[test]
    fn format_function() {
        let input = "
add_library(abc PUBLIC
    # leading comment
    path/to/.cpp # trailing comment
    \"path with space\"
)";
        let expected = "add_library(
\tabc
\tPUBLIC
\t\t# leading comment
\t\tpath/to/.cpp  # trailing comment
\t\t\"path with space\"
)
";
        let formatted = format_str(input);
        assert_eq!(expected, formatted);
    }

    #[test]
    fn preserve_newlines() {
        let input = "set(A B)\n\nset(C D)";
        let expected = "set(A B)\n\nset(C D)\n";
        let formatted = format_str(input);
        assert_eq!(expected, formatted);
    }

    #[test]
    fn install_targets() {
        let input = "
install(
    TARGETS
            MeshToolkit
    RUNTIME
            DESTINATION MeshToolkit/bin
    LIBRARY
            DESTINATION MeshToolkit/bin
)";
        let expected = "install(
\tTARGETS
\t\tMeshToolkit
\tRUNTIME
\t\tDESTINATION MeshToolkit/bin
\tLIBRARY
\t\tDESTINATION MeshToolkit/bin
)\n";
        let formatted = format_str(input);
        assert_eq!(expected, formatted);
    }

    #[test]
    fn install_directory() {
        let input = "
install(
    DIRECTORY
        ${MW_REPOSITORY_DIR}/5axis/customer/IntegrationSamples
    DESTINATION .
    PATTERN \".owner.py\"
    EXCLUDE
)";
        let expected = "install(
\tDIRECTORY
\t\t${MW_REPOSITORY_DIR}/5axis/customer/IntegrationSamples
\tDESTINATION .
\tPATTERN \".owner.py\"
\t\tEXCLUDE
)\n";
        let formatted = format_str(input);
        assert_eq!(expected, formatted);
    }

    #[test]
    fn get_property() {
        let input = "get_property(variable_with_long_name_to_force_line_break DIRECTORY some_directory PROPERTY some_property)";
        let expected = "get_property(
\tvariable_with_long_name_to_force_line_break
\tDIRECTORY some_directory
\tPROPERTY some_property
)\n";
        let formatted = format_str(input);
        assert_eq!(expected, formatted);
    }

    #[test]
    fn get_property_optional_dir() {
        let input = "get_property(variable_with_long_name_to_force_line_break DIRECTORY PROPERTY some_property)";
        let expected = "get_property(
\tvariable_with_long_name_to_force_line_break
\tDIRECTORY
\tPROPERTY some_property
)\n";
        let formatted = format_str(input);
        assert_eq!(expected, formatted);
    }
}
