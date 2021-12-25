type desc = List of expr list | Symbol of string | Int of int
and expr = { desc : desc; loc : Location.t }

let rec print_expr ppf x =
  match x.desc with
  | List l ->
      Format.fprintf ppf "@[<1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print_expr)
        l
  | Symbol s -> Format.pp_print_string ppf s
  | Int n -> Format.pp_print_int ppf n

let rec expr toks =
  match toks with
  | Lexer.LPAREN :: toks ->
      let rec loop accu toks =
        match toks with
        | Lexer.RPAREN :: toks ->
            ({ desc = List (List.rev accu); loc = Location.none }, toks)
        | _ ->
            let x, toks = expr toks in
            loop (x :: accu) toks
      in
      loop [] toks
  | QUOTE :: toks ->
      let x, toks = expr toks in
      ( {
          desc = List [ { desc = Symbol "quote"; loc = Location.none }; x ];
          loc = Location.none;
        },
        toks )
  | INT s :: toks ->
      ({ desc = Int (int_of_string s); loc = Location.none }, toks)
  | SYMBOL s :: toks -> ({ desc = Symbol s; loc = Location.none }, toks)
  | _ -> failwith "syntax error"

let parse lexbuf =
  let rec loop toks =
    match Lexer.token lexbuf with
    | Lexer.EOF -> List.rev toks
    | tok -> loop (tok :: toks)
  in
  let toks = loop [] in
  let x, toks = expr toks in
  if toks <> [] then failwith "too much input";
  x
