{
type desc =
  | INT of string
  | LPAREN
  | RPAREN
  | QUOTE
  | SYMBOL of string
  | FALSE
  | TRUE

type token =
  {
    desc : desc;
    loc : Location.t;
  }

let lexeme_loc lexbuf =
  {
    Location.loc_start = Lexing.lexeme_start_p lexbuf;
    loc_end = Lexing.lexeme_end_p lexbuf;
    loc_ghost = false;
  }

let mk lexbuf desc =
  Some { desc ; loc = lexeme_loc lexbuf }
}

rule token = parse
| [' ''\t']+
    { token lexbuf }
| '\n'
    { Lexing.new_line lexbuf; token lexbuf }
| '('
    { mk lexbuf LPAREN }
| ')'
    { mk lexbuf RPAREN }
| '\''
    { mk lexbuf QUOTE }
| "#f"
    { mk lexbuf FALSE }
| "#t"
    { mk lexbuf TRUE }
| ['0'-'9']+ as s
    { mk lexbuf (INT s) }
| ['a'-'z''+''?''!']+ as s
    { mk lexbuf (SYMBOL s) }
| ';' [^'\n']*
    { token lexbuf }
| eof
    { None }
