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
  (diff quote.lambda quote.lambda.output)))

(rule
 (alias quote)
 (action
  (diff quote.expected quote.output)))

(rule
 (target typerr1.exe)
 (action (run %{ocamlc} -o %{target} %{lib-private:znscheme_lib:znscheme_lib.cma} %{dep:typerr1.cmo})))

(rule
 (targets typerr1.cmo typerr1.lambda.output)
 (deps (package znscheme_lib))
 (action
  (run znscheme -stdlib %{lib:znscheme_lib:znscheme_lib.cma} -dlambda typerr1.lambda.output %{dep:typerr1.scm})))

(rule
 (with-stdout-to typerr1.output (run ./typerr1.exe)))

(rule
 (alias typerr1)
 (action
  (diff typerr1.lambda typerr1.lambda.output)))

(rule
 (alias typerr1)
 (action
  (diff typerr1.expected typerr1.output)))

(alias
 (name runtest)
 (deps
  (alias quote)
  (alias typerr1)))
