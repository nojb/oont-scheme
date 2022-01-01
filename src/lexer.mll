{
type desc =
  | Int of string
  | Lparen
  | Rparen
  | Quote
  | Quasiquote
  | Unquote
  | Unquote_splicing
  | Atom of string
  | False
  | True

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
    { mk lexbuf Lparen }
| ')'
    { mk lexbuf Rparen }
| '\''
    { mk lexbuf Quote }
| '`'
    { mk lexbuf Quasiquote }
| ','
    { mk lexbuf Unquote }
| ",@"
    { mk lexbuf Unquote_splicing }
| "#f"
    { mk lexbuf False }
| "#t"
    { mk lexbuf True }
| ['0'-'9']+ as s
    { mk lexbuf (Int s) }
| ['a'-'z''+''?''!']+ as s
    { mk lexbuf (Atom s) }
| ';' [^'\n']*
    { token lexbuf }
| "#|"
    { comment 0 lexbuf }
| eof
    { None }

and comment n = parse
| "|#"
    { if n = 0 then token lexbuf else comment (n - 1) lexbuf }
| "#|"
    { comment (n+1) lexbuf }
| _
    { comment n lexbuf }
