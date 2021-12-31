module L = Lambda_helper
module P = Prepare

let drawlambda = ref false
let dlambda = ref false
let compile_only = ref false
let output_name = ref ""
let stdlib_ident = Ident.create_persistent "Oont"

module Helpers = struct
  let falsev = L.int 0b01
  let truev = L.int 0b11
  let intv n = L.int (n lsl 1)
  let untag_int n = L.lsrint n (L.int 1)
  let tag_int n = L.lslint n (L.int 1)
  let stringv ~loc s = L.string ~loc s
  let emptylist = L.int 0b111
  let undefined = L.int 0b1111
  let prim name = L.value "Oont" name
  let boolv b = if b then truev else falsev
  let tag_bool x = L.ifthenelse x truev falsev
  let error_exn = lazy (L.extension_constructor "Oont" "Error")
  let cons car cdr = L.makemutable 0 [ car; cdr ]
  let errorv ~loc s objs = L.makeblock 5 (L.string ~loc s :: objs)

  let type_error obj =
    L.raise
      (L.makeblock 0
         [
           Lazy.force error_exn; errorv ~loc:Location.none "Type error" [ obj ];
         ])

  let checkint n =
    L.letin n (fun id ->
        let v = L.var id in
        L.ifthenelse
          (L.sequor (L.not (L.isint v)) (L.eq (L.andint v (L.int 1)) (L.int 1)))
          (type_error v) (L.int 0))

  let listv xs =
    List.fold_left (fun cdr x -> cons x cdr) emptylist (List.rev xs)

  let if_ x1 x2 x3 = L.ifthenelse (L.eq x1 falsev) x2 x3

  let toint x =
    L.letin x (fun id ->
        let v = L.var id in
        L.seq (checkint v) (untag_int v))

  let apply f args =
    L.letin f (fun id ->
        let f = L.var id in
        let doit =
          let arity = L.field f 0 in
          let clos = L.field f 2 in
          let nargs = if args = [] then 1 else List.length args in
          L.ifthenelse
            (L.eq (L.int nargs) arity)
            (L.apply clos (if args = [] then [ L.int 0 ] else args))
            (type_error f)
        in
        L.seq
          (L.ifthenelse (L.isint f) (type_error f) (L.int 0))
          (L.block_switch f [ (4, doit) ] (Some (type_error f))))
end

let get_sym s = L.apply (Helpers.prim "sym") [ L.string s ]
let num_errors = ref 0

let prerr_errorf ?loc fmt =
  incr num_errors;
  Printf.ksprintf
    (fun s ->
      Location.print_report Format.err_formatter (Location.error ?loc s);
      L.int 0)
    fmt

let comp_primitive p args =
  match (p, args) with
  | P.Pzerop, [ x ] ->
      L.letin x (fun id ->
          let v = L.var id in
          L.seq (Helpers.checkint v) (Helpers.tag_bool (L.eq v (L.int 0))))
  | Paddint, [] -> Helpers.intv 0
  | Paddint, x :: xs ->
      Helpers.tag_int
        (List.fold_left
           (fun accu x ->
             let n = Helpers.toint x in
             L.addint accu n)
           (Helpers.toint x) xs)
  | Pcons, [ car; cdr ] -> Helpers.cons car cdr
  | Psym s, [] -> get_sym s
  | _ -> invalid_arg "comp_primitive"

let rec comp_expr { P.desc; loc = _ } =
  match desc with
  | Const (Const_int n) -> Helpers.intv n
  | Const (Const_bool b) -> Helpers.boolv b
  | Const Const_emptylist -> Helpers.emptylist
  | Apply (f, args) -> Helpers.apply (comp_expr f) (List.map comp_expr args)
  | Var { txt; _ } -> Lvar txt
  | If (e1, e2, e3) ->
      let e3 =
        match e3 with None -> Helpers.undefined | Some e3 -> comp_expr e3
      in
      Helpers.if_ (comp_expr e1) (comp_expr e2) e3
  | Prim (p, args) -> comp_primitive p (List.map comp_expr args)
  | Lambda (args, _extra, body) ->
      let args =
        if args = [] then [ Ident.create_local "dummy" ]
        else List.map (fun { Location.txt; _ } -> txt) args
      in
      L.makeblock 4
        [ L.int (List.length args); L.string ""; L.func args (comp_expr body) ]
  | Begin [] -> Helpers.undefined
  | Begin (e :: es) ->
      List.fold_left (fun accu e -> L.seq accu (comp_expr e)) (comp_expr e) es

let to_bytecode ~required_globals fname lam =
  let bname = Filename.remove_extension (Filename.basename fname) in
  let modname = String.capitalize_ascii bname in
  let cmofile = Filename.remove_extension fname ^ ".cmo" in
  let code = Bytegen.compile_implementation modname lam in
  let oc = open_out_bin cmofile in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> Emitcode.to_file oc modname cmofile ~required_globals code);
  cmofile

let parse_file fname =
  let ic = open_in_bin fname in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let lexbuf = Lexing.from_channel ic in
      Location.input_name := fname;
      Location.init lexbuf fname;
      Location.input_lexbuf := Some lexbuf;
      Parser.parse_sexp_list lexbuf)

let process_file fname =
  let sexps = parse_file fname in
  let e = P.parse_expr_list P.initial_env sexps in
  let lam = comp_expr e in
  let lam = L.apply (Helpers.prim "print") [ lam ] in
  if !drawlambda then Format.eprintf "@[%a@]@." Printlambda.lambda lam;
  if !num_errors = 0 then (
    let lam = Simplif.simplify_lambda lam in
    if !dlambda then Format.eprintf "@[%a@]@." Printlambda.lambda lam;
    let required_globals = Ident.Set.singleton stdlib_ident in
    Some (to_bytecode ~required_globals fname lam))
  else None

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
  Clflags.debug := true;
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
