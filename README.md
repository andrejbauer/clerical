# Clerical

An implementation of an imperative langauge for exact real number computation.

## Prerequisites

To compile the OCaml implementation of Clerical you need OCaml 5 or later.
You also need the following:

* the [MFPR](http://www.mpfr.org) library
* the [Dune](https://dune.build) build system
* the [Menhir](http://gallium.inria.fr/~fpottier/menhir/) OCaml parser generator
* the Ocaml libraries `menhirLib`, `sedlex` and `mlgmpidl`

### OCaml & OPAM

Follow [these instructions](https://www.ocaml.org/docs/up-and-running) for
installing OCaml 5 and the OCaml package manager OPAM.

It is possible to have several OCaml versions installed in parallel (these are
called “switches”). Make sure that you switch to OCaml 5 using the command:

    opam switch 5.0.0


### MPFR

The GNU multiple-precision floating-point library [MPFR]((http://www.mpfr.org))
is aviable through various package managers. On MacOS you can install it using
[Homebrew](https://brew.sh):

    brew install mpfr

### OCaml tools & libraries

First install OPAM, as described above. Verify that OCaml 5 is activated by running

    opam switch

You should see something like, noting the arrow `→` indicating the active version:

    #  switch   compiler                    description
       4.10.0   ocaml-base-compiler.4.10.0  4.10.0
       4.12.0   ocaml-base-compiler.4.12.0  4.12.0
    →  5.0.0    ocaml-base-compiler.5.0.0   5.0.0
       default  ocaml-system.4.09.0         default

If Ocaml 5 is not active, you can activate it with

    opam switch 5.0.0

possibly replacing `5.0.0` with a later version (use `opam switch
list-available` to see what is available).

Install OCaml tools and libraries with

    opam install dune
    opam install menhir
    opam install sedlex
    opam install mlgmpidl

**Note:** At the time of writing (2023-03-12) OPAM does not yet support
`mlgmpidl` for OCaml 5, even though the package works with OCaml 5. You might have to install it manually from the GitHub repository [`mlgmpidl`](https://github.com/nberth/mlgmpidl).


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
