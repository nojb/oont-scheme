type datum_desc = List of datum list | Quote of datum | Int of int
and datum = { desc : datum_desc }

val parse : Lexing.lexbuf -> datum
