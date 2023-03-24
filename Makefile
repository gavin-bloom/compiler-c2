.SUFFIXES:
.PHONY: all clean

all: c2

lex.ml: lex.mll
	ocamllex lex.mll

parse.mli parse.ml: parse.mly
	ocamlyacc -v parse.mly

c2: x86.ml cfg.ml spill.ml interf.ml regalloc.ml compile.ml c2.ml
c2: ast.ml parse.mli parse.ml lex.ml typecheck.ml eval.ml
	ocamlc -g -o $@ $^

clean:
	rm -fv *.o *.cmo *.cmi
	rm -fv parse.output parse.ml parse.mli lex.ml
	rm -fv c2

