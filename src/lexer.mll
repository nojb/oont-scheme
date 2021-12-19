{
type token =
  | INT of string
  | EOF
  | LPAREN
  | RPAREN
  | QUOTE
  | SYMBOL of string
}

rule token = parse
| [' ''\t']+
    { token lexbuf }
| '\n'
    { Lexing.new_line lexbuf; token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '\''
    { QUOTE }
| ['0'-'9']+ as s
    { INT s }
| ['a'-'z''+']+ as s
    { SYMBOL s }
| eof
    { EOF }
