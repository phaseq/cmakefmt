use nom::branch::alt;
use nom::bytes::complete::{tag, take_till, take_till1, take_while1};
use nom::sequence::delimited;
use nom::IResult;

const SCOPE_BEGIN: &[&str] = &[
    "if", "else", "elseif", "while", "foreach", "function", "macro",
];
const SCOPE_END: &[&str] = &[
    "else",
    "elseif",
    "endif",
    "endwhile",
    "endforeach",
    "endfunction",
    "endmacro",
];

// for some commands we don't want the auto-nesting behavior: all arguments should be shown on the same level
const DISABLE_NESTING_FUNCTIONS: &[&str] = &[
    "option",
    "set",
    "foreach",
    "list",
    "string",
    "file",
    "add_library",
    "add_executable",
];

const DISABLE_INLINE_FUNCTIONS: &[&str] = &[
    "target_compile_definitions",
    "target_compile_features",
    "target_compile_options",
    "target_include_directories",
    "target_link_directories",
    "target_link_libraries",
    "target_link_options",
    "target_precompile_headers",
    "target_sources",
];

struct Cli {
    help: bool,
    in_place: bool,
    path: String,
}
impl Cli {
    fn from_env() -> Option<Cli> {
        let mut args = pico_args::Arguments::from_env();
        Some(Cli {
            help: args.contains(["-h", "--help"]),
            in_place: args.contains("-i"),
            path: args.free().ok()?.get(0)?.clone(),
        })
    }

    fn print_help() {
        println!(
            "USAGE: {} [options] <file>\n",
            std::env::args().next().unwrap()
        );
        println!("OPTIONS:");
        println!("  -i  - Inplace edit <file>, if specified");
    }
}

fn main() -> std::io::Result<()> {
    let args = match Cli::from_env() {
        Some(args) if args.help => {
            Cli::print_help();
            std::process::exit(0);
        }
        Some(args) => args,
        None => {
            Cli::print_help();
            std::process::exit(1);
        }
    };

    let input = std::fs::read_to_string(&args.path)?;
    let result = format_str(&input);
    if args.in_place {
        std::fs::write(&args.path, result)?;
    } else {
        println!("{}", result);
    }

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
                if indent_n == 0 {
                    eprintln!("ERROR: too many closed scopes");
                    std::process::exit(1);
                }
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
            println!("result so far: {}", result);
            panic!("remainder: {:?}", input);
        }
    }
    result
}

fn format_function(function: &Function, indent_n: usize) -> String {
    let n_indent = if allow_inline(function, indent_n) {
        None
    } else {
        Some(indent_n + 1)
    };
    let mut args = String::new();
    format_function_args(&mut args, &function.args, n_indent, &function.name);

    let indent = "\t".repeat(indent_n);
    if n_indent.is_some() {
        format!("{}{}({}\n{})", indent, function.name, args, indent)
    } else {
        format!("{}{}({})", indent, function.name, args)
    }
}

fn allow_inline(function: &Function, indent_n: usize) -> bool {
    if DISABLE_INLINE_FUNCTIONS.contains(&function.name) {
        return false;
    }
    let len = function_args_len(&function.args).map(|l| l + function.name.len() + 2);
    match len {
        Some(len) => {
            if len < 80 - 4 * indent_n {
                true
            } else {
                false
            }
        }
        None => false,
    }
}

fn format_function_args(
    mut result: &mut String,
    args: &[Arg],
    indent_n: Option<usize>,
    function_name: &str,
) {
    if args.is_empty() {
        return;
    }
    match indent_n {
        Some(indent_n) => {
            let sections = section_args(args, function_name);
            let inline = false;
            for section in &sections {
                format_function_section(&mut result, section, indent_n, inline, function_name);
            }
        }
        None => {
            for (i, arg) in args.iter().enumerate() {
                if i != 0 {
                    result.push(' ');
                }
                match arg {
                    Arg::Keyword(v) => result.push_str(v),
                    Arg::Unquoted(v) => result.push_str(v),
                    Arg::Quoted(v) => result.push_str(&format!("\"{}\"", v)),
                    Arg::Comment(_) => unreachable!(),
                    Arg::TrailingComment(_) => unreachable!(),
                    Arg::Braced(v) => {
                        result.push('(');
                        format_function_args(&mut result, v, None, function_name);
                        result.push(')');
                    }
                }
            }
        }
    };
}

fn format_function_section(
    mut result: &mut String,
    section: &Section,
    indent_n: usize,
    inline: bool,
    function_name: &str,
) {
    format_function_arg(&mut result, section.header, indent_n, inline, function_name);

    let allow_inline = !DISABLE_INLINE_FUNCTIONS.contains(&function_name);
    let inline = allow_inline && section.members.len() == 1;
    let next_indent = if inline { indent_n } else { indent_n + 1 };
    for section in &section.members {
        format_function_section(&mut result, section, next_indent, inline, function_name);
    }
}

fn format_function_arg(
    mut result: &mut String,
    arg: &Arg,
    indent_n: usize,
    inline: bool,
    function_name: &str,
) {
    let indent = if inline {
        " ".to_string()
    } else {
        "\n".to_string() + &"\t".repeat(indent_n)
    };
    match arg {
        Arg::Unquoted(v) | Arg::Keyword(v) | Arg::Comment(v) => {
            result.push_str(&format!("{}{}", indent, v))
        }
        Arg::Quoted(v) => result.push_str(&format!("{}\"{}\"", indent, v)),
        Arg::TrailingComment(v) => result.push_str(&format!("  {}", v)),
        Arg::Braced(v) => {
            result.push_str(&indent);
            result.push('(');
            format_function_args(&mut result, v, Some(indent_n + 1), function_name);
            result.push_str(&indent);
            result.push(')');
        }
    }
}

#[derive(Clone, Debug)]
struct Section<'a> {
    header: &'a Arg<'a>,
    members: Vec<Section<'a>>,
}
impl<'a> Section<'a> {
    fn new(header: &'a Arg<'a>) -> Section {
        Section {
            header,
            members: vec![],
        }
    }
}
fn section_args<'a>(args: &'a [Arg<'a>], function_name: &'a str) -> Vec<Section<'a>> {
    let indented = indent_args(args, function_name);
    //println!("indented: {:?}", indented);
    let result = section_args_recurse(&indented, 0, 0).0;
    //println!("{:#?}", result);
    result
}

fn section_args_recurse<'a, 'b>(
    args: &'b [IndentArg<'a>],
    cur_indent: usize,
    mut cur_idx: usize,
) -> (Vec<Section<'a>>, usize) {
    let mut result: Vec<Section> = vec![];
    while cur_idx < args.len() {
        let &(arg, indent) = &args[cur_idx];
        use std::cmp::Ordering;
        match indent.cmp(&cur_indent) {
            Ordering::Equal => {
                result.push(Section::new(arg));
                cur_idx += 1;
            }
            Ordering::Greater => {
                let (members, next_idx) = section_args_recurse(args, cur_indent + 1, cur_idx);
                cur_idx = next_idx;
                result.last_mut().unwrap().members = members;
            }
            Ordering::Less => {
                break;
            }
        }
    }
    (result, cur_idx)
}

type IndentArg<'a> = (&'a Arg<'a>, usize);
fn indent_args<'a>(args: &'a [Arg<'a>], function_name: &'a str) -> Vec<IndentArg<'a>> {
    let keyword_opens_scope: Box<dyn Fn(&str) -> bool> = match function_name {
        // for some commands we give the nesting arguments explicitly
        "target_compile_definitions"
        | "target_compile_features"
        | "target_compile_options"
        | "target_include_directories"
        | "target_link_directories"
        | "target_link_libraries"
        | "target_link_options"
        | "target_precompile_headers" => {
            Box::new(|arg| ["PUBLIC", "PRIVATE", "INTERFACE"].contains(&arg))
        }

        // some functions should never be nested
        _ if DISABLE_NESTING_FUNCTIONS.contains(&function_name) => Box::new(|_arg| false),

        // by default: open a scope if the argument has the format of SOME_KEYWORD
        _ => Box::new(|_arg| true),
    };
    let keyword_opens_nested_scope: &[&'static str] = match args.get(0) {
        // handle nested scopes for this command:
        // https://cmake.org/cmake/help/latest/command/install.html
        //
        // install(TARGETS targets... [EXPORT <export-name>]
        //     [[ARCHIVE|LIBRARY|RUNTIME|OBJECTS|FRAMEWORK|BUNDLE|
        //       PRIVATE_HEADER|PUBLIC_HEADER|RESOURCE]
        //      [DESTINATION <dir>]
        //      [PERMISSIONS permissions...]
        //      [CONFIGURATIONS [Debug|Release|...]]
        //      [COMPONENT <component>]
        //      [NAMELINK_COMPONENT <component>]
        //      [OPTIONAL] [EXCLUDE_FROM_ALL]
        //      [NAMELINK_ONLY|NAMELINK_SKIP]
        //     ] [...]
        //     [INCLUDES DESTINATION [<dir> ...]]
        // )
        Some(Arg::Keyword("TARGETS")) if function_name == "install" => &[
            "DESTINATION",
            "PERMISSIONS",
            "CONFIGURATIONS",
            "COMPONENT",
            "NAMELINK_COMPONENT",
            "NAMELINK_SKIP",
        ],

        // handle nested scopes for this command:
        // install(DIRECTORY dirs...
        //     TYPE <type> | DESTINATION <dir>
        //     [FILE_PERMISSIONS permissions...]
        //     [DIRECTORY_PERMISSIONS permissions...]
        //     [USE_SOURCE_PERMISSIONS] [OPTIONAL] [MESSAGE_NEVER]
        //     [CONFIGURATIONS [Debug|Release|...]]
        //     [COMPONENT <component>] [EXCLUDE_FROM_ALL]
        //     [FILES_MATCHING]
        //     [[PATTERN <pattern> | REGEX <regex>]
        //      [EXCLUDE] [PERMISSIONS permissions...]] [...])
        _ if function_name == "install" => &["EXCLUDE"],
        _ => &[],
    };

    let mut result: Vec<IndentArg> = vec![];
    let mut cur_indent = 0;
    for arg in args {
        match arg {
            Arg::Keyword(v) if keyword_opens_scope(v) => {
                let level = if keyword_opens_nested_scope.contains(v) {
                    1
                } else {
                    0
                };
                result.push((arg, level));
                cur_indent = level + 1;
            }
            _ => {
                result.push((arg, cur_indent));
            }
        }
    }
    result
}

fn function_args_len(args: &[Arg]) -> Option<usize> {
    if args.is_empty() {
        return Some(0);
    }
    let mut sum = 0usize;
    for arg in args {
        match arg {
            Arg::Keyword(v) => sum += v.len(),
            Arg::Unquoted(v) => sum += v.len(),
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
    Keyword(&'a str),
    Unquoted(&'a str),
    Quoted(&'a str),
    Comment(&'a str),
    TrailingComment(&'a str),
    Braced(Vec<Arg<'a>>),
}

fn parse_function<'a>(input: &'a str) -> IResult<&'a str, Function<'a>> {
    let (input, name) = parse_unquoted_arg(input)?;
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
        } else if let Ok((i, c)) = parse_keyword(input) {
            input = i;
            r.push(Arg::Keyword(c));
        } else if let Ok((i, t)) = parse_unquoted_arg(input) {
            input = i;
            r.push(Arg::Unquoted(t));
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

fn parse_keyword(input: &str) -> IResult<&str, &str> {
    let (input, _) = nom::combinator::not(tag("MW_"))(input)?;
    let (input, keyword) = take_while1(|c| ('A'..='Z').contains(&c) || "_".contains(c))(input)?;
    alt((parse_space, tag(")")))(input)?;
    Ok((input, keyword))
}

fn parse_unquoted_arg(input: &str) -> IResult<&str, &str> {
    //take_while1(|c| nom::AsChar::is_alphanum(c) || "_.${}-<>=/\\".contains(c))(input)
    take_till1(|c| " \t\n\r()".contains(c))(input)
}

fn parse_string(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("\"")(input)?;
    let mut skip_delimiter = false;
    for (i, ch) in input.char_indices() {
        if ch == '\\' && !skip_delimiter {
            skip_delimiter = true;
        } else if ch == '"' && !skip_delimiter {
            return Ok((&input[i + 1..], &input[..i]));
        } else {
            skip_delimiter = false;
        }
    }
    Err(nom::Err::Incomplete(nom::Needed::Unknown))
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
\tTARGETS MeshToolkit
\tRUNTIME DESTINATION MeshToolkit/bin
\tLIBRARY DESTINATION MeshToolkit/bin
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
\tDIRECTORY ${MW_REPOSITORY_DIR}/5axis/customer/IntegrationSamples
\tDESTINATION .
\tPATTERN
\t\t\".owner.py\"
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

    #[test]
    fn option() {
        let input = "if(1)option(
            MW_RELEASECUT_INSTALL_MWSUPPORT
                    ${MW_SUPPORT_OPTION_DESCRIPTION}
                    OFF)endif()";
        let expected = "if(1)
\toption(
\t\tMW_RELEASECUT_INSTALL_MWSUPPORT
\t\t${MW_SUPPORT_OPTION_DESCRIPTION}
\t\tOFF
\t)
endif()\n";
        let formatted = format_str(input);
        assert_eq!(expected, formatted);
    }

    #[test]
    fn conditions() {
        let input = "if(NOT ( \"${_type}\" STREQUAL \"INTERFACE_LIBRARY\"))endif()";
        let expected = "if(NOT (\"${_type}\" STREQUAL \"INTERFACE_LIBRARY\"))
endif()\n";
        let formatted = format_str(input);
        assert_eq!(expected, formatted);
    }

    #[test]
    fn string() {
        let input = "file(WRITE ${_license_bin_file} \"# Licenses for incorporated software in ModuleWorks ${MW_INSTALL_PACKAGE}\\n\\n\")\r\n";
        let expected = "file(
\tWRITE
\t${_license_bin_file}
\t\"# Licenses for incorporated software in ModuleWorks ${MW_INSTALL_PACKAGE}\\n\\n\"
)\n";
        let formatted = format_str(input);
        assert_eq!(expected, formatted);
    }

    #[test]
    fn cpack() {
        let input = r#"file(APPEND "${_package_file}"
        "set(CPACK_PACKAGE_NAME \"${_package_name}\")\n"
        "set(CPACK_PACKAGE_DESCRIPTION \"N/A\")\n"
        "set(CPACK_INSTALLED_DIRECTORIES \"${MW_PACKAGES_DIR}/${_package_name};.\")\n"
        "set(CPACK_GENERATOR \"${CPACK_GENERATOR}\")\n"
        "set(CPACK_PACKAGE_VERSION \"${CPACK_PACKAGE_VERSION}\")\n"
        "set(CPACK_PACKAGE_FILE_NAME \"${CPACK_PACKAGE_FILE_NAME}\")\n")"#;
        let expected = "file(
\tAPPEND
\t\"${_package_file}\"
\t\"set(CPACK_PACKAGE_NAME \\\"${_package_name}\\\")\\n\"
\t\"set(CPACK_PACKAGE_DESCRIPTION \\\"N/A\\\")\\n\"
\t\"set(CPACK_INSTALLED_DIRECTORIES \\\"${MW_PACKAGES_DIR}/${_package_name};.\\\")\\n\"
\t\"set(CPACK_GENERATOR \\\"${CPACK_GENERATOR}\\\")\\n\"
\t\"set(CPACK_PACKAGE_VERSION \\\"${CPACK_PACKAGE_VERSION}\\\")\\n\"
\t\"set(CPACK_PACKAGE_FILE_NAME \\\"${CPACK_PACKAGE_FILE_NAME}\\\")\\n\"
)\n";
        let formatted = format_str(input);
        assert_eq!(expected, formatted);
    }
}
