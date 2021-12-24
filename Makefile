.PHONY: all
all:
	dune build @all

.PHONY: clean
clean:
	dune clean

.PHONY: runtime
runtime:
	ocamlc -I runtime -c runtime/schemeStdlib.mli runtime/schemeStdlib.ml
