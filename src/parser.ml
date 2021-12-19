type datum_desc = List of datum list | Symbol of string | Int of int
and datum = { desc : datum_desc }

let rec print_datum ppf x =
  match x.desc with
  | List l ->
      Format.fprintf ppf "@[<1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print_datum)
        l
  | Symbol s -> Format.pp_print_string ppf s
  | Int n -> Format.pp_print_int ppf n

let rec datum toks =
  match toks with
  | Lexer.LPAREN :: toks ->
      let rec loop accu toks =
        match toks with
        | Lexer.RPAREN :: toks -> ({ desc = List (List.rev accu) }, toks)
        | _ ->
            let x, toks = datum toks in
            loop (x :: accu) toks
      in
      loop [] toks
  | QUOTE :: toks ->
      let x, toks = datum toks in
      ({ desc = List [ { desc = Symbol "quote" }; x ] }, toks)
  | INT s :: toks -> ({ desc = Int (int_of_string s) }, toks)
  | SYMBOL s :: toks -> ({ desc = Symbol s }, toks)
  | _ -> failwith "syntax error"

let parse lexbuf =
  let rec loop toks =
    match Lexer.token lexbuf with
    | Lexer.EOF -> List.rev toks
    | tok -> loop (tok :: toks)
  in
  let toks = loop [] in
  let x, toks = datum toks in
  if toks <> [] then failwith "too much input";
  x
