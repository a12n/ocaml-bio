OCAMLBUILD_FLAGS =
OCAMLBUILD_FLAGS += -use-ocamlfind


.PHONY: clean lib test utop

lib:
	ocamlbuild ${OCAMLBUILD_FLAGS} bio.cma

clean:
	ocamlbuild ${OCAMLBUILD_FLAGS} -clean
	${RM} *_tests.ml qtest.targets.log

test: lib

utop: lib
	utop -I _build -safe-string


%_tests.ml: %.ml
	qtest -o $@ extract $<
