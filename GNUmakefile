OCAMLBUILD_FLAGS =
OCAMLBUILD_FLAGS += -use-ocamlfind

LIB = bio
TARGET ?= byte

.PHONY: clean install lib lib-byte lib-native test uninstall utop

lib: lib-byte lib-native

lib-byte:
	ocamlbuild ${OCAMLBUILD_FLAGS} ${LIB}.cma

lib-native:
	ocamlbuild ${OCAMLBUILD_FLAGS} ${LIB}.cmxa

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

utop: lib-byte
	utop -I _build -init utop_init.ml -safe-string

install: lib
	ocamlfind install ${LIB}	\
		*.mli	\
		META	\
		_build/*.a	\
		_build/*.cma	\
		_build/*.cmi	\
		_build/*.cmx	\
		_build/*.cmxa

uninstall:
	ocamlfind remove ${LIB}


%.byte: %.ml
	ocamlbuild ${OCAMLBUILD_FLAGS} $@

%.d.byte: %.ml
	ocamlbuild ${OCAMLBUILD_FLAGS} $@

%.native: %.ml
	ocamlbuild ${OCAMLBUILD_FLAGS} $@

%_tests.ml: %.ml
	qtest -o $@ extract $<
