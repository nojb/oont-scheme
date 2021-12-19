{
type token =
  | INT of string
  | EOF
  | LPAREN
  | RPAREN
  | QUOTE
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
| eof
    { EOF }
