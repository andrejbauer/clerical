DUNE=dune

default: clerical.exe

COQMAKEFILE = coq_makefile
COQSRC = formalization

OCAMLBUILD = ocamlbuild
OCAMLBUILD_FLAGS = -j 4 -use-ocamlfind -pkg menhirLib -pkg $(SEDLEX) -pkg gmp -pkg zarith
OCAMLBUILD_MENHIRFLAGS = -use-menhir -menhir "menhir --explain"
SRCDIR = src

.PHONY: coq_code clean clerical.exe

### Compilation of Coq files

$(COQSRC)/Makefile: $(COQSRC)/_CoqProject
	cd $(COQSRC) && $(COQMAKEFILE) -f _CoqProject

coq_code: $(COQSRC)/Makefile
	$(MAKE) -C $(COQSRC)

### Compilation of OCaml files

src/build.ml:
	/bin/echo -n 'let version = "' > $@
	$(MAKE) -s version | tr -d '\n' >> $@
	/bin/echo '" ;;' >> $@

clerical.exe: src/build.ml
	$(DUNE) build src/clerical.exe

# Cleaning up

clean: $(COQSRC)/Makefile
	$(MAKE) -C $(COQSRC) clean
	$(DUNE) clean
