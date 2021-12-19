type datum_desc = List of datum list | Quote of datum | Int of int
and datum = { desc : datum_desc }

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
      ({ desc = Quote x }, toks)
  | INT s :: toks -> ({ desc = Int (int_of_string s) }, toks)
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
