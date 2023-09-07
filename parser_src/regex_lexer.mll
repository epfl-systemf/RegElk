{
open Regex_parser
open Lexing
}

rule token = parse
| ['a'-'z'] as c { CHAR c }
| ['0'-'9']+ { DIGITS (int_of_string (Lexing.lexeme lexbuf)) }
| '|' { ALT }
| '(' { LPAR }
| ')' { RPAR }
| '[' { LBRACK }
| ']' { RBRACK }
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
| '=' { EQUAL }
| '!' { EXCL }
| "\\b" { WORDBOUND }
| "\\B" { NONWORDBOUND }
| eof { EOF }