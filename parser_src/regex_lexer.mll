{
open Regex_parser
open Lexing

exception SyntaxError of string
}

let digit = ['0'-'9']
let syntaxcharacter = ['^' '$' '\\' '.' '*' '+' '?' '(' ')' '[' ']' '{' '}' '|']
let patterncharacter = _#syntaxcharacter
let backref = '\\'digit

rule token = parse
| digit as d { DIGIT d }
| '|' { ALT }
| '(' { LPAR }
| ')' { RPAR }
| '[' { LBRACK }
| ']' { RBRACK }
| '{' { LBRAC }
| '}' { RBRAC }
| ',' { COMMA }
| '*' { STAR }
| '+' { PLUS }
| '?' { QMARK }
| '^' { HAT }
| '$' { DOLLAR }
| '\\' { BACKSL }
| '.' { DOT }
| ':' { COLONS }
| '<' { LESS }
| '>' { MORE }
| '=' { EQUAL }
| '-' { MINUS }
| '!' { EXCL }
| "\\b" { WORDBOUND }
| "\\B" { NONWORDBOUND }
| "\\d" { DIGITCLASS }
| "\\D" { NONDIGITCLASS }
| "\\s" { SPACECLASS }
| "\\S" { NONSPACECLASS }
| "\\w" { WORDCLASS }
| "\\W" { NONWORDCLASS }
| "\\f" { FORMFEED }
| "\\n" { NEWLINE }
| "\\r" { CARRIAGE }
| "\\t" { TAB }
| "\\v" { raise (SyntaxError "Unsupported ControlEscape: vertical tab") }
| "\\k" { raise (SyntaxError "Named Capture Groups are unsupported") }
| "\\x" { raise (SyntaxError "Hex Escape unsupported") }
| "\\u" { raise (SyntaxError "Unicode Escape unsupported") }
| "\\p" { raise (SyntaxError "Unicode Property unsupported") }
| backref { raise (SyntaxError "Backreferences unsupported") }
| patterncharacter as c { CHAR c }
| eof { EOF }