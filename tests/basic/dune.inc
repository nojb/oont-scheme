;; This file is generated.

(rule
 (target quote.exe)
 (action (run %{ocamlc} -o %{target} %{lib-private:znscheme_lib:znscheme_lib.cma} %{dep:quote.cmo})))

(rule
 (targets quote.cmo quote.lambda.output)
 (deps (package znscheme_lib))
 (action
  (run znscheme -stdlib %{lib:znscheme_lib:znscheme_lib.cma} -dlambda quote.lambda.output %{dep:quote.scm})))

(rule
 (with-stdout-to quote.output (run ./quote.exe)))

(rule
 (alias quote)
 (action
  (progn
   (diff quote.lambda quote.lambda.output)
   (diff quote.expected quote.output))))

(alias
 (name runtest)
 (deps
  (alias quote)))
