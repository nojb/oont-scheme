let test_scm fn = Filename.check_suffix fn ".scm"

let print_stanza fn =
  let bn = Filename.remove_extension fn in
  Printf.printf
    {|
(rule
 (target %s.exe)
 (action (run %%{ocamlc} -o %%{target} %%{lib-private:znscheme_lib:znscheme_lib.cma} %%{dep:%s.cmo})))

(rule
 (targets %s.cmo %s.lambda.output)
 (deps (package znscheme_lib))
 (action
  (run znscheme -stdlib %%{lib:znscheme_lib:znscheme_lib.cma} -dlambda %s.lambda.output %%{dep:%s})))

(rule
 (with-stdout-to %s.output (run ./%s.exe)))

(rule
 (alias %s)
 (action
  (progn
   (diff %s.lambda %s.lambda.output)
   (diff %s.expected %s.output))))
|}
    bn bn bn bn bn fn bn bn bn bn bn bn bn

let () =
  print_endline ";; This file is generated.";
  let tests =
    Sys.readdir Filename.current_dir_name |> Array.to_seq |> Seq.filter test_scm
  in
  Seq.iter print_stanza tests;
  print_string {|
(alias
 (name runtest)
 (deps|};
  Seq.iter
    (fun fn -> Printf.printf "\n  (alias %s)" (Filename.remove_extension fn))
    tests;
  print_string "))\n"
