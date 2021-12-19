type datum_desc = List of datum list | Symbol of string | Int of int
and datum = { desc : datum_desc }

val print_datum : Format.formatter -> datum -> unit
val parse : Lexing.lexbuf -> datum
