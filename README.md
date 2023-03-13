# Clerical

An implementation of an imperative langauge for exact real number computation.

## Prerequisites

To compile the OCaml implementation of Clerical and a sufficiently new version of OCaml (version 4.10 or later should work).
You also need the following:

* the [MFPR](http://www.mpfr.org) library
* the [Dune](https://dune.build) build system
* the [Menhir](http://gallium.inria.fr/~fpottier/menhir/) OCaml parser generator
* the Ocaml libraries `menhirLib`, `sedlex` and `mlgmpidl`

### OCaml & OPAM

Follow [these instructions](https://www.ocaml.org/docs/up-and-running) for
installing OCaml and the OCaml package manager OPAM.


### MPFR

The GNU multiple-precision floating-point library [MPFR]((http://www.mpfr.org))
is aviable through various package managers. On MacOS you can install it using
[Homebrew](https://brew.sh):

    brew install mpfr

### OCaml tools & libraries

Install OCaml tools and libraries with

    opam install dune
    opam install menhir
    opam install sedlex
    opam install mlgmpidl

**Note:** At the time of writing (2023-03-12) OPAM does not yet support `mlgmpidl` for OCaml 5, even though the code compiles with OCaml 5. You might have to install it manually from the GitHub repository [`mlgmpidl`](https://github.com/nberth/mlgmpidl) if you are using OCaml 5.


## Compilation

To compile Clerical, run the following command in the Clerical directory:

    dune build

Dune compiles the program and hides the executable in `_build/default/src/clerical.exe`, so try running it with

    _build/default/src/clerical.exe --prelude prelude.real


## Repository structure

The structure of the repository:

* `src` – the OCaml implementation of Clerical
* `examples` – examples of Clerical programs
* `doc` – documentation

## Clerical syntax

Please consult:

* [`doc/syntax.md`](doc/syntax.md) for a brief explanation of the syntax
* [`examples`](./examples) for examples of Clerical programs
* [`prelude.real`)(./prelude.real) for the built-in functions and operators
