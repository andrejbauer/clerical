# Figure out which version of OCaml is being used
OCAML_VERSION=$(shell ocamlc --version)

# Figure out which version of sedlex is being used
SEDLEX_VERSION=$(shell opam info sedlex --version)

# Set up correct incantation for sedlex
SEDLEX=$(shell if [ "$(SEDLEX_VERSION)" \< "2.0" ] ; then echo "sedlex" ; else echo "sedlex.ppx"; fi)

COQMAKEFILE = coq_makefile
COQSRC = formalization

OCAMLBUILD = ocamlbuild
OCAMLBUILD_FLAGS = -j 4 -use-ocamlfind -pkg menhirLib -pkg $(SEDLEX) -pkg gmp -pkg zarith
OCAMLBUILD_MENHIRFLAGS = -use-menhir -menhir "menhir --explain"
SRCDIR = src

default: clerical.native

.PHONY: coq_code clean clerical.byte clerical.native clerical.d.byte clerical.p.native

### Compilation of Coq files

$(COQSRC)/Makefile: $(COQSRC)/_CoqProject
	cd $(COQSRC) && $(COQMAKEFILE) -f _CoqProject

coq_code: $(COQSRC)/Makefile
	$(MAKE) -C $(COQSRC)

### Compilation of OCaml files

clerical.byte clerical.native clerical.d.byte clerical.p.native: src/build.ml
	ocamlbuild $(OCAMLBUILD_MENHIRFLAGS) $(OCAMLBUILD_FLAGS) $@

src/build.ml:
	/bin/echo -n 'let version = "' > $@
	$(MAKE) -s version | tr -d '\n' >> $@
	/bin/echo '" ;;' >> $@

# Cleaning up

clean:
	if [-f $(COQSRC)/Makefile] ; then $(MAKE) -C $(COQSRC) clean; fi
	$(OCAMLBUILD) -clean
