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

type token = { desc : desc; loc : Location.t }

val token : Lexing.lexbuf -> token
(** Raises [End_of_file]. *)
