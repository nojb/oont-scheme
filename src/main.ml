let () =
  let lexbuf = Lexing.from_channel stdin in
  let datum = Parser.parse lexbuf in
  let lam =
    match datum with
    | { Parser.desc = Quote datum } ->
        let rec quote = function
          | { Parser.desc = List datums } ->
              let rec cons cdr = function
                | x :: xs -> cons (Lambda.Const_block (0, [ quote x; cdr ])) xs
                | [] -> cdr
              in
              cons (Lambda.const_int 0) (List.rev datums)
          | { desc = Int n } -> Lambda.const_int n
          | _ -> assert false
        in
        Lambda.Lconst (quote datum)
    | _ -> assert false
  in
  Printlambda.lambda Format.std_formatter lam
