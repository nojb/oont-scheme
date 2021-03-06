type desc =
  | List of sexp list
  | Atom of string
  | Int of int
  | Bool of bool
  | Vector of sexp list
  | String of string

and sexp = { sexp_desc : desc; sexp_loc : Location.t }

val print_sexp : Format.formatter -> sexp -> unit
val parse_sexp_list : Lexing.lexbuf -> sexp list
