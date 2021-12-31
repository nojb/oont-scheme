module L = Lambda_helper
module P = Prepare

let drawlambda = ref false
let dlambda = ref false
let compile_only = ref false
let output_name = ref ""
let stdlib_ident = Ident.create_persistent "Oont"
let num_errors = ref 0

let prerr_errorf ?loc fmt =
  incr num_errors;
  Printf.ksprintf
    (fun s ->
      Location.print_report Format.err_formatter (Location.error ?loc s);
      L.int 0)
    fmt

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
  let lam = Lambdagen.comp_expr e in
  let lam = L.apply (Lambdagen.prim "print") [ lam ] in
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
