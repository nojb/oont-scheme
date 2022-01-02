{
type desc =
  | INT of string
  | LPAREN
  | HASHLPAREN
  | RPAREN
  | QUOTE
  | BACKQUOTE
  | COMMA
  | COMMAAT
  | ATOM of string
  | FALSE
  | TRUE
  | STRING of string

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
  { desc; loc = lexeme_loc lexbuf }
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
| "#("
    { mk lexbuf HASHLPAREN }
| '\''
    { mk lexbuf QUOTE }
| '`'
    { mk lexbuf BACKQUOTE }
| ','
    { mk lexbuf COMMA }
| ",@"
    { mk lexbuf COMMAAT }
| "#f"
    { mk lexbuf FALSE }
| "#t"
    { mk lexbuf TRUE }
| ['0'-'9']+ as s
    { mk lexbuf (INT s) }
| ['a'-'z''+''?''!''-']+ as s
    { mk lexbuf (ATOM s) }
| ';' [^'\n']*
    { token lexbuf }
| "#|"
    { comment 0 lexbuf }
| '"'
    { let buf = Buffer.create 0 in string buf lexbuf }
| eof
    { raise End_of_file }

and string buf = parse
| '"'
    { { desc = STRING (Buffer.contents buf); loc = Location.none } }
| _ as c
    { Buffer.add_char buf c; string buf lexbuf }

and comment n = parse
| "|#"
    { if n = 0 then token lexbuf else comment (n - 1) lexbuf }
| "#|"
    { comment (n+1) lexbuf }
| _
    { comment n lexbuf }
