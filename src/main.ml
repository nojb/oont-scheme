module L = Lambda

let stdlib = Ident.create_persistent "SchemeStdlib"

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
                      (Pandint, [ Lvar id; Lconst (L.const_int 1) ], Loc_unknown);
                    Lconst (L.const_int 0);
                  ],
                  Loc_unknown ),
              Lprim (Plsrint, [ Lvar id; Lconst (L.const_int 1) ], Loc_unknown),
              Lprim
                ( Praise Raise_regular,
                  [
                    Lprim
                      ( Pmakeblock (0, Immutable, None),
                        [
                          Lprim
                            ( Pfield 0,
                              [ Lprim (Pgetglobal stdlib, [], Loc_unknown) ],
                              Loc_unknown );
                          Lconst (Const_immstring "Type error");
                        ],
                        Loc_unknown );
                  ],
                  Loc_unknown ) ),
          Lprim
            ( Praise Raise_regular,
              [
                Lprim
                  ( Pmakeblock (0, Immutable, None),
                    [
                      Lprim
                        ( Pfield 0,
                          [ Lprim (Pgetglobal stdlib, [], Loc_unknown) ],
                          Loc_unknown );
                      Lconst (Const_immstring "Type error");
                    ],
                    Loc_unknown );
              ],
              Loc_unknown ) ) )

let rec comp datum =
  match datum with
  | { Parser.desc = List [ { desc = Symbol "quote" }; x ] } ->
      let rec quote = function
        | { Parser.desc = List datums } ->
            let rec cons cdr = function
              | x :: xs -> cons (L.Const_block (0, [ quote x; cdr ])) xs
              | [] -> cdr
            in
            cons (L.const_int 0) (List.rev datums)
        | { desc = Int n } -> L.const_int (n lsl 1)
        | _ -> assert false
      in
      L.Lconst (quote x)
  | { desc = Int n } -> Lconst (L.const_int (n lsl 1))
  | { desc = List [ { desc = Symbol "+" }; x1; x2 ] } ->
      let n1 = extract_int (comp x1) in
      let n2 = extract_int (comp x2) in
      L.Lprim
        ( Plslint,
          [ Lprim (Paddint, [ n1; n2 ], Loc_unknown); Lconst (L.const_int 1) ],
          Loc_unknown )
  | x ->
      Format.eprintf ">>> @[%a@]@." Parser.print_datum x;
      assert false

let to_bytecode fname lam =
  let bname = Filename.remove_extension (Filename.basename fname) in
  let modname = String.capitalize_ascii bname in
  let cmofile = Filename.remove_extension fname ^ ".cmo" in
  let oc = open_out_bin cmofile in
  Simplif.simplify_lambda lam
  |> Bytegen.compile_implementation modname
  |> Emitcode.to_file oc modname cmofile
       ~required_globals:(Ident.Set.singleton stdlib)

let process fname =
  let ic = open_in_bin fname in
  let datum =
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let lexbuf = Lexing.from_channel ic in
        Parser.parse lexbuf)
  in
  let lam = comp datum in
  let lam =
    L.Lapply
      {
        ap_func =
          Lprim
            ( Pfield 1,
              [ Lprim (Pgetglobal stdlib, [], Loc_unknown) ],
              Loc_unknown );
        ap_args = [ lam ];
        ap_loc = Loc_unknown;
        ap_tailcall = Default_tailcall;
        ap_inlined = Default_inline;
        ap_specialised = Default_specialise;
      }
  in
  Format.printf "@[%a@]@." Printlambda.lambda lam;
  to_bytecode fname lam

let () = Arg.parse [] process ""
