# Clerical

An implementation of an imperative langauge for real number computation.

## Prerequisites

### Coq formalization

To compile the Coq formalization you just need a fairly recent version of Coq.
The code compiles with Coq 8.6, but it is likely that older versions are ok.

### Clerical implementation

To compile the OCaml implementation of Clerical you need OCaml.
The code is known to compile with OCaml 4.07.1 and 4.09.0.
You also need the following OCaml libraries:

* the [MFPR](http://www.mpfr.org) library
* the `menhir` OCaml parser generator
* the `sedlex` OCaml package
* the `mlgmpidl` OCaml package, version 1.2.6 or later.
* the `zarith` OCaml package

MPFR is aviable through various package managers. On macOS you can install it using
[Homebrew](https://brew.sh):

    brew install mpfr

It is easy to get the OCaml dependencies with [OPAM](https://opam.ocaml.org), the OCaml
package manager. First install OPAM, for example on macOS you can use Homebrew:

    brew install opam

Then install OCaml dependencies:

    opam install dune
    opam install menhir
    opam install sedlex
    opam install mlgmpidl
    opam install zarith

## Compilation instructions

To compile the Coq code, run

    make coq_code

To compile Clerical, run

    make clerical.native


## Repository structure

The structure of the repository:

* `src` – the OCaml implementation of Clerical
* `formalization` – the Coq formalization of the language
* `examples` – examples of Clerical programs
* `doc` – documentation

## Clerical syntax

Please consult:

* [`doc/syntax.md`](doc/syntax.md) for a brief explanation of the syntax
* [`examples`](./examples) for examples of Clerical programs
* [`prelude.real`)(./prelude.real) for the built-in functions and operators
