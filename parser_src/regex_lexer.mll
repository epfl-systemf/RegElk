{
open Regex_parser
open Lexing

exception Unsupported_Vtab
exception Unsupported_named_groups
exception Unsupported_hex
exception Unsupported_unicode
exception Unsupported_prop
exception Unsupported_backref

exception SyntaxError of string
}

let digit = ['0'-'9']
let syntaxcharacter = ['^' '$' '\\' '.' '*' '+' '?' '(' ')' '[' ']' '{' '}' '|']
let patterncharacter = _#syntaxcharacter
let backref = '\\'['1'-'9']

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
| "\\0" { NULL }
| "\\v" { raise Unsupported_Vtab }
| "\\k" { raise Unsupported_named_groups }
| "\\x" { raise Unsupported_hex }
| "\\u" { raise Unsupported_unicode }
| "\\p" { raise Unsupported_prop }
| backref { raise Unsupported_backref }
| patterncharacter as c { CHAR c }
| eof { EOF }