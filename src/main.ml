open Lambda

let drawlambda = ref false
let dlambda = ref false
let compile_only = ref false
let output_name = ref ""
let stdlib_ident = Ident.create_persistent "Oont"

module Helpers = struct
  let falsev = Lconst (const_int 1)
  let intv n = Lconst (const_int (n lsl 1))
  let unsafe_toint n = Lprim (Plsrint, [ n; Lconst (const_int 1) ], Loc_unknown)
  let unsafe_ofint n = Lprim (Plslint, [ n; Lconst (const_int 1) ], Loc_unknown)
  let stringv ~loc s = Lconst (Const_base (Const_string (s, loc, None)))
  let emptylist = Lconst (const_int 0b111)
  let undefined = Lconst (const_int 0b1111)
  let stdlib_prim = Lambda.transl_prim "Oont"

  let error_exn =
    lazy
      (let env = Env.add_persistent_structure stdlib_ident Env.empty in
       let lid = Longident.Ldot (Longident.Lident "Oont", "Error") in
       match Env.find_constructor_by_name lid env with
       | { cstr_tag = Cstr_extension (path, _); _ } ->
           transl_extension_path Loc_unknown env path
       | _ -> Misc.fatal_error "Error extension not found."
       | exception Not_found -> Misc.fatal_error "Error extension not found.")

  let cons car cdr =
    Lprim (Pmakeblock (0, Mutable, None), [ car; cdr ], Loc_unknown)

  let listv xs =
    List.fold_left (fun cdr x -> cons x cdr) emptylist (List.rev xs)

  let errorv ~loc s objs =
    Lprim
      ( Pmakeblock (4, Immutable, None),
        [ stringv ~loc s; listv objs ],
        Loc_unknown )

  let if_ x1 x2 x3 =
    Lifthenelse (Lprim (Pintcomp Ceq, [ x1; falsev ], Loc_unknown), x2, x3)

  let type_error obj =
    Lprim
      ( Praise Raise_regular,
        [
          Lprim
            ( Pmakeblock (0, Immutable, None),
              [
                Lazy.force error_exn;
                errorv ~loc:Location.none "Type error" [ obj ];
              ],
              Loc_unknown );
        ],
        Loc_unknown )

  let toint lam =
    name_lambda Strict lam (fun id ->
        Lifthenelse
          ( Lprim (Pisint, [ Lvar id ], Loc_unknown),
            Lifthenelse
              ( Lprim (Pandint, [ Lvar id; Lconst (const_int 1) ], Loc_unknown),
                type_error (Lvar id),
                unsafe_toint (Lvar id) ),
            type_error (Lvar id) ))

  let apply _ _ = assert false
end

module Env : sig
  type data =
    | Psyntax of (loc:Location.t -> t -> Parser.expr list -> lambda)
    | Pvar of Ident.t
    | Pprim of (loc:Location.t -> lambda list -> lambda)

  and t

  val empty : t
  val find : string -> t -> data option

  val add_syntax :
    string -> (loc:Location.t -> t -> Parser.expr list -> lambda) -> t -> t

  val add_prim : string -> (loc:Location.t -> lambda list -> lambda) -> t -> t
end = struct
  module Map = Map.Make (String)

  type data =
    | Psyntax of (loc:Location.t -> t -> Parser.expr list -> lambda)
    | Pvar of Ident.t
    | Pprim of (loc:Location.t -> lambda list -> lambda)

  and t = { env : data Map.t }

  let empty = { env = Map.empty }
  let find s t = Map.find_opt s t.env
  let add_syntax s f t = { env = Map.add s (Psyntax f) t.env }
  let add_prim s f t = { env = Map.add s (Pprim f) t.env }
end

let syms = Hashtbl.create 0
let symnames = ref []

let get_sym s =
  match Hashtbl.find_opt syms s with
  | None ->
      let id = Ident.create_local s in
      Hashtbl.add syms s id;
      symnames := (s, id) :: !symnames;
      id
  | Some id -> id

let num_errors = ref 0

let prerr_errorf ?loc fmt =
  incr num_errors;
  Printf.ksprintf
    (fun s ->
      Location.print_report Format.err_formatter (Location.error ?loc s);
      Lconst const_unit)
    fmt

let rec comp env { Parser.desc; loc } =
  match desc with
  | List ({ desc = Symbol s; loc } :: args) -> (
      match Env.find s env with
      | Some (Psyntax f) -> f ~loc env args
      | Some (Pvar id) -> Helpers.apply (Lvar id) (List.map (comp env) args)
      | Some (Pprim f) -> f ~loc (List.map (comp env) args)
      | None -> prerr_errorf ~loc "%s: not found" s)
  | Int n -> Helpers.intv n
  | List (f :: args) -> Helpers.apply (comp env f) (List.map (comp env) args)
  | List [] -> prerr_errorf ~loc "missing procedure"
  | Symbol s -> (
      match Env.find s env with
      | Some (Psyntax _) -> prerr_errorf ~loc "%s: bad syntax" s
      | Some (Pvar id) -> Lvar id
      | Some (Pprim _) -> assert false (* eta-expand *)
      | None -> prerr_errorf ~loc "%s: not found" s)

let add_prim ~loc:_ = function
  | [] -> Helpers.intv 0
  | x :: xs ->
      Helpers.unsafe_ofint
        (List.fold_left
           (fun accu x ->
             let n = Helpers.toint x in
             Lprim (Paddint, [ accu; n ], Loc_unknown))
           (Helpers.toint x) xs)

let quote_syntax ~loc _ = function
  | [ x ] ->
      let rec quote { Parser.desc; loc = _ } =
        match desc with
        | List xs -> Helpers.listv (List.map quote xs)
        | Int n -> Helpers.intv n
        | Symbol s -> Lvar (get_sym s)
      in
      quote x
  | [] -> prerr_errorf ~loc "quote: not enough arguments"
  | _ :: _ :: _ -> prerr_errorf ~loc "quote: too many arguments"

let if_syntax ~loc env = function
  | [ x1; x2 ] -> Helpers.if_ (comp env x1) (comp env x2) Helpers.undefined
  | [ x1; x2; x3 ] -> Helpers.if_ (comp env x1) (comp env x2) (comp env x3)
  | _ -> prerr_errorf ~loc "if: bad number of arguments"

let initial_env =
  Env.add_syntax "if" if_syntax
    (Env.add_syntax "quote" quote_syntax (Env.add_prim "+" add_prim Env.empty))

let to_bytecode fname lam =
  let bname = Filename.remove_extension (Filename.basename fname) in
  let modname = String.capitalize_ascii bname in
  let scofile = Filename.remove_extension fname ^ ".sco" in
  let lam = Simplif.simplify_lambda lam in
  if !dlambda then Format.eprintf "@[%a@]@." Printlambda.lambda lam;
  let code = Bytegen.compile_implementation modname lam in
  let oc = open_out_bin scofile in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      Emitcode.to_file oc modname scofile
        ~required_globals:(Ident.Set.singleton stdlib_ident)
        code);
  scofile

let lapply ap_func ap_args =
  Lapply
    {
      ap_func;
      ap_args;
      ap_loc = Loc_unknown;
      ap_tailcall = Default_tailcall;
      ap_inlined = Default_inline;
      ap_specialised = Default_specialise;
    }

let parse_file fname =
  let ic = open_in_bin fname in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let lexbuf = Lexing.from_channel ic in
      Location.input_name := fname;
      Location.init lexbuf fname;
      Location.input_lexbuf := Some lexbuf;
      Parser.parse lexbuf)

let process_file fname =
  let e = parse_file fname in
  let lam = comp initial_env e in
  let lam =
    List.fold_left
      (fun lam (s, id) ->
        Llet
          ( Strict,
            Pgenval,
            id,
            lapply (Helpers.stdlib_prim "sym") [ Lconst (Const_immstring s) ],
            lam ))
      lam (List.rev !symnames)
  in
  let lam = lapply (Helpers.stdlib_prim "print") [ lam ] in
  if !drawlambda then Format.eprintf "@[%a@]@." Printlambda.lambda lam;
  if !num_errors = 0 then Some (to_bytecode fname lam) else None

let spec =
  [
    ("-drawlambda", Arg.Set dlambda, " Dump IR (before simplif)");
    ("-dlambda", Arg.Set dlambda, " Dump IR (after simplif)");
    ("-c", Arg.Set compile_only, " Only compile, do not link");
    ("-o", Arg.Set_string output_name, " Set output name");
  ]

let fnames = ref []

let main () =
  Arg.parse (Arg.align spec) (fun fn -> fnames := fn :: !fnames) "";
  let libdir =
    Filename.concat
      (Filename.concat
         (Filename.dirname (Filename.dirname Sys.executable_name))
         "lib")
      "oont"
  in
  Clflags.include_dirs := libdir :: !Clflags.include_dirs;
  Compmisc.init_path ();
  let fnames = List.rev !fnames in
  let obj_names = List.filter_map process_file fnames in
  if !num_errors = 0 && not !compile_only then (
    let output_name =
      match (!output_name, fnames) with
      | "", [ fn ] -> Filename.remove_extension fn ^ ".exe"
      | "", _ :: _ :: _ -> failwith "Must specify -o"
      | s, _ -> s
    in
    Compmisc.init_path ();
    Bytelink.link ("oont.cma" :: obj_names) output_name)

let () =
  try main () with exn -> Location.report_exception Format.err_formatter exn
