module L = Lambda

let extract_int lam =
  let id = Ident.create_local "n" in
  L.Llet
    ( Strict,
      Pgenval,
      id,
      lam,
      Lifthenelse
        ( Lprim (Pisint, [ Lvar id ], Loc_unknown),
          Lifthenelse
            ( Lprim
                ( Pintcomp Ceq,
                  [
                    Lprim
                      ( Pandint,
                        [ Lvar id; Lconst (Lambda.const_int 1) ],
                        Loc_unknown );
                    Lconst (Lambda.const_int 0);
                  ],
                  Loc_unknown ),
              Lprim
                (Plsrint, [ Lvar id; Lconst (Lambda.const_int 1) ], Loc_unknown),
              Lprim
                ( Praise Raise_regular,
                  [ Lconst (Const_immstring "Type error") ],
                  Loc_unknown ) ),
          Lprim
            ( Praise Raise_regular,
              [ Lconst (Const_immstring "Type error") ],
              Loc_unknown ) ) )

let rec comp datum =
  match datum with
  | { Parser.desc = List [ { desc = Symbol "quote" }; x ] } ->
      let rec quote = function
        | { Parser.desc = List datums } ->
            let rec cons cdr = function
              | x :: xs -> cons (Lambda.Const_block (0, [ quote x; cdr ])) xs
              | [] -> cdr
            in
            cons (Lambda.const_int 0) (List.rev datums)
        | { desc = Int n } -> Lambda.const_int (n lsl 1)
        | _ -> assert false
      in
      Lambda.Lconst (quote x)
  | { desc = Int n } -> Lconst (Lambda.const_int (n lsl 1))
  | { desc = List [ { desc = Symbol "+" }; x1; x2 ] } ->
      let n1 = extract_int (comp x1) in
      let n2 = extract_int (comp x2) in
      L.Lprim (Paddint, [ n1; n2 ], Loc_unknown)
  | x ->
      Format.eprintf ">>> @[%a@]@." Parser.print_datum x;
      assert false

let () =
  let lexbuf = Lexing.from_channel stdin in
  let datum = Parser.parse lexbuf in
  let lam = comp datum in
  Format.printf "@[%a@]@." Printlambda.lambda lam
