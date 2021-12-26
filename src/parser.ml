type desc = List of expr list | Symbol of string | Int of int | Bool of bool
and expr = { desc : desc; loc : Location.t }

let merge_loc { Location.loc_start; _ } { Location.loc_end; _ } =
  { Location.loc_start; loc_end; loc_ghost = false }

let rec print_expr ppf x =
  match x.desc with
  | List l ->
      Format.fprintf ppf "@[<1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print_expr)
        l
  | Symbol s -> Format.pp_print_string ppf s
  | Int n -> Format.pp_print_int ppf n
  | Bool true -> Format.pp_print_string ppf "#t"
  | Bool false -> Format.pp_print_string ppf "#f"

let rec expr toks =
  match toks with
  | { Lexer.desc = LPAREN; loc = loc1 } :: toks ->
      let rec loop accu toks =
        match toks with
        | { Lexer.desc = RPAREN; loc = loc2 } :: toks ->
            ({ desc = List (List.rev accu); loc = merge_loc loc1 loc2 }, toks)
        | _ ->
            let x, toks = expr toks in
            loop (x :: accu) toks
      in
      loop [] toks
  | { desc = QUOTE; loc } :: toks ->
      let x, toks = expr toks in
      ( {
          desc = List [ { desc = Symbol "quote"; loc }; x ];
          loc = merge_loc loc x.loc;
        },
        toks )
  | { desc = INT s; loc } :: toks ->
      ({ desc = Int (int_of_string s); loc }, toks)
  | { desc = SYMBOL s; loc } :: toks -> ({ desc = Symbol s; loc }, toks)
  | { desc = FALSE; loc } :: toks -> ({ desc = Bool false; loc }, toks)
  | { desc = TRUE; loc } :: toks -> ({ desc = Bool true; loc }, toks)
  | _ -> failwith "syntax error"

let parse lexbuf =
  let rec loop toks =
    match Lexer.token lexbuf with
    | None -> List.rev toks
    | Some tok -> loop (tok :: toks)
  in
  let toks = loop [] in
  let x, toks = expr toks in
  if toks <> [] then failwith "too much input";
  x
