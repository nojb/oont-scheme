type desc = List of expr list | Symbol of string | Int of int | Bool of bool
and expr = { desc : desc; loc : Location.t }

val print_expr : Format.formatter -> expr -> unit
val parse : Lexing.lexbuf -> expr
