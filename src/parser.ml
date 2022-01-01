type desc =
  | List of sexp list
  | Atom of string
  | Int of int
  | Bool of bool
  | Vector of sexp list
  | String of string

and sexp = { desc : desc; loc : Location.t }

let merge_loc { Location.loc_start; _ } { Location.loc_end; _ } =
  { Location.loc_start; loc_end; loc_ghost = false }

let rec print_sexp ppf x =
  match x.desc with
  | List sexpl ->
      Format.fprintf ppf "@[<1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print_sexp)
        sexpl
  | Atom s -> Format.pp_print_string ppf s
  | Int n -> Format.pp_print_int ppf n
  | Bool true -> Format.pp_print_string ppf "#t"
  | Bool false -> Format.pp_print_string ppf "#f"
  | String s -> Format.fprintf ppf "%S" s
  | Vector sexpl ->
      Format.fprintf ppf "@[<2>#(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print_sexp)
        sexpl

let rec parse_sexp toks =
  let { Lexer.desc; loc }, toks =
    match toks with [] -> failwith "syntax error" | tok :: toks -> (tok, toks)
  in
  match desc with
  | RPAREN -> failwith "invalid syntax"
  | LPAREN ->
      let rec loop accu toks =
        match toks with
        | { Lexer.desc = RPAREN; loc = loc2 } :: toks ->
            ({ desc = List (List.rev accu); loc = merge_loc loc loc2 }, toks)
        | _ ->
            let x, toks = parse_sexp toks in
            loop (x :: accu) toks
      in
      loop [] toks
  | HASHLPAREN ->
      let rec loop accu toks =
        match toks with
        | { Lexer.desc = RPAREN; loc = loc2 } :: toks ->
            ({ desc = Vector (List.rev accu); loc = merge_loc loc loc2 }, toks)
        | _ ->
            let x, toks = parse_sexp toks in
            loop (x :: accu) toks
      in
      loop [] toks
  | QUOTE ->
      let x, toks = parse_sexp toks in
      ( {
          desc = List [ { desc = Atom "quote"; loc }; x ];
          loc = merge_loc loc x.loc;
        },
        toks )
  | BACKQUOTE ->
      let x, toks = parse_sexp toks in
      ( {
          desc = List [ { desc = Atom "quasiquote"; loc }; x ];
          loc = merge_loc loc x.loc;
        },
        toks )
  | COMMA ->
      let x, toks = parse_sexp toks in
      ( {
          desc = List [ { desc = Atom "unquote"; loc }; x ];
          loc = merge_loc loc x.loc;
        },
        toks )
  | COMMAAT ->
      let x, toks = parse_sexp toks in
      ( {
          desc = List [ { desc = Atom "unquote-splicing"; loc }; x ];
          loc = merge_loc loc x.loc;
        },
        toks )
  | INT s -> ({ desc = Int (int_of_string s); loc }, toks)
  | ATOM s -> ({ desc = Atom s; loc }, toks)
  | FALSE -> ({ desc = Bool false; loc }, toks)
  | TRUE -> ({ desc = Bool true; loc }, toks)
  | STRING s -> ({ desc = String s; loc }, toks)

let parse_sexp_list lexbuf =
  let rec loop toks =
    match Lexer.token lexbuf with
    | exception End_of_file -> List.rev toks
    | tok -> loop (tok :: toks)
  in
  let toks = loop [] in
  let rec loop sexps = function
    | [] -> List.rev sexps
    | toks ->
        let x, toks = parse_sexp toks in
        loop (x :: sexps) toks
  in
  loop [] toks
