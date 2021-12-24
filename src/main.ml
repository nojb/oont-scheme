module L = Lambda

let stdlib = Ident.create_persistent "SchemeStdlib"

module Helpers = struct
  let falsev = L.Lconst (L.const_int 1)

  let if_ x1 x2 x3 =
    L.Lifthenelse (Lprim (Pintcomp Ceq, [ x1; falsev ], Loc_unknown), x2, x3)
end

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

let syms = Hashtbl.create 0

let get_sym s =
  match Hashtbl.find_opt syms s with
  | None ->
      let id = Ident.create_local s in
      Hashtbl.add syms s id;
      id
  | Some id -> id

let rec comp datum =
  match datum with
  | { Parser.desc = List [ { desc = Symbol "quote" }; x ] } ->
      let rec quote = function
        | { Parser.desc = List datums } ->
            let rec cons cdr = function
              | x :: xs ->
                  cons
                    (L.Lprim
                       ( Pmakeblock (0, Immutable, None),
                         [ quote x; cdr ],
                         Loc_unknown ))
                    xs
              | [] -> cdr
            in
            cons (L.Lconst (L.const_int 0)) (List.rev datums)
        | { desc = Int n } -> L.Lconst (L.const_int (n lsl 1))
        | { desc = Symbol s } -> L.Lvar (get_sym s)
      in
      quote x
  | { desc = Int n } -> Lconst (L.const_int (n lsl 1))
  | { desc = List [ { desc = Symbol "if" }; x1; x2; x3 ] } ->
      Helpers.if_ (comp x1) (comp x2) (comp x3)
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

let lapply ap_func ap_args =
  L.Lapply
    {
      ap_func;
      ap_args;
      ap_loc = Loc_unknown;
      ap_tailcall = Default_tailcall;
      ap_inlined = Default_inline;
      ap_specialised = Default_specialise;
    }

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
    Hashtbl.fold
      (fun sym id lam ->
        L.Llet
          ( Strict,
            Pgenval,
            id,
            lapply
              (Lprim
                 ( Pfield 1,
                   [ Lprim (Pgetglobal stdlib, [], Loc_unknown) ],
                   Loc_unknown ))
              [ Lconst (Const_immstring sym) ],
            lam ))
      syms lam
  in
  let lam =
    lapply
      (Lprim
         (Pfield 2, [ Lprim (Pgetglobal stdlib, [], Loc_unknown) ], Loc_unknown))
      [ lam ]
  in
  Format.printf "@[%a@]@." Printlambda.lambda lam;
  to_bytecode fname lam

let () = Arg.parse [] process ""
