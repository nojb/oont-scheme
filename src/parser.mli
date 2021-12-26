type desc = List of sexp list | Symbol of string | Int of int | Bool of bool
and sexp = { desc : desc; loc : Location.t }

val print_sexp : Format.formatter -> sexp -> unit
val parse_sexp_list : Lexing.lexbuf -> sexp list
