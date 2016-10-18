OCAMLBUILD_FLAGS =
OCAMLBUILD_FLAGS += -use-ocamlfind

TARGET ?= byte

.PHONY: clean lib test utop

lib:
	ocamlbuild ${OCAMLBUILD_FLAGS} bio.cma

clean:
	ocamlbuild ${OCAMLBUILD_FLAGS} -clean
	${RM} *_tests.ml qtest.targets.log

test:	\
		aa_tests.${TARGET}	\
		dna_tests.${TARGET}	\
		prot_tests.${TARGET}	\
		rna_tests.${TARGET}
	@for test in $^; do	\
		echo ==== $$test ====;	\
		./$$test --slow 5;	\
	done

utop: lib
	utop -I _build -init utop_init.ml -safe-string


%.byte: %.ml
	ocamlbuild ${OCAMLBUILD_FLAGS} $@

%.d.byte: %.ml
	ocamlbuild ${OCAMLBUILD_FLAGS} $@

%.native: %.ml
	ocamlbuild ${OCAMLBUILD_FLAGS} $@

%_tests.ml: %.ml
	qtest -o $@ extract $<
