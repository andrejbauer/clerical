# Clerical

An implementation of an imperative langauge for real number computation.

## Prerequisites

### Coq files

To compile the Coq formalization you just need a fairly recent version of Coq.
The code compiles with Coq 8.6, but it is likely that older versions are ok.

### Clerical implementation

To compile the OCaml implementation of Clerical you need OCaml and

* the [MFPR](http://www.mpfr.org) library
* the `menhir` OCaml parser generator
* the `sedlex` OCaml package
* the `mlgmpidl` OCaml package

MPFR is aviable through various package managers. On OSX you can install it using
[Homebrew](https://brew.sh):

    brew install mpfr

It is easy to get the OCaml dependencies with [OPAM]:

    opam install menhir
    opam install sedlex
    opam install mlgmpidl

## Compilation instructions

To compile the Coq code, run

    make coq_code

To compile Clerical, run

    make clerical.native


## Repository structure

The structure of the repository:

* `src` – the OCaml implementation of Clerical
* `coq` – the Coq formalization of the language
* `doc` – various documents
