# Clerical

An implementation of an imperative langauge for exact real number computation.

## Prerequisites

To compile the OCaml implementation of Clerical and a sufficiently new version of OCaml (version 5.20 or later should work).
You also need the following:

* the [MFPR](http://www.mpfr.org) library
* the [Dune](https://dune.build) build system
* the [Menhir](http://gallium.inria.fr/~fpottier/menhir/) OCaml parser generator
* the Ocaml libraries `menhirLib`, `sedlex`, `mlgmpidl` and `picos`


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

    opam install dune menhir sedlex mlgmpidl

### Picos library

As of 2025-04-28 we need [Picos](https://github.com/ocaml-multicore/picos) library version 0.6 or later. If you are far enough in the future,
you can install these with

    opam install picos_std picos_mux

If not, you will have to use the development version from the repository. You get these by pinning them,
as follows:

    opam pin add picos     git+ssh://git@github.com/ocaml-multicore/picos.git
    opam pin add picos_aux git+ssh://git@github.com/ocaml-multicore/picos.git
    opam pin add picos_mux git+ssh://git@github.com/ocaml-multicore/picos.git
    opam pin add picos_std git+ssh://git@github.com/ocaml-multicore/picos.git

Good luck!

## Compilation

To compile Clerical, run the following command in the Clerical directory:

    dune build

Dune compiles the program and creates the executable `clerical.exe` (also on MacOS and Linux). You can run it with:

    ./clerical.exe --prelude prelude.real


## Unit testing

Dune unit tests can be run with

    dune runtest

and validate tests with

    dune promote

See [Writting and running tests](https://dune.readthedocs.io/en/stable/tests.html) section of Dune documentation for
further information on unit testing.

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

## How the parallel features work

We outline here how Clerical uses cooperative multi-threading, effects, and handlers.

The evaluation of a Clerical expression is represented by a *thread*. When the thread is started, it receives two parameters: *working
precision* `p` and *loop fuel* `f`. It performs MPFR operations at precision `p`, and it runs `while` loops for at most `f` iterations. A thread may peform the following actions:

* It may terminate with a resulting value `v`.
* It may perform the operation `Yield`, indicating that another thread can run. Every thread does this periodically.
* If it experiences loss of precision or it runs out of fuel, it performs the operation `Resign`. If the thread is resumed, it will restart computations with a higher working precision, and will give itself more fuel to complete any ongoing loops.

A suspended thread may be discontinued by passing it the `Abort` exception.

### Guarded case

The guarded `case` runs all the cases as separate threads, using a simple round-robin scheduler. It keeps a queue of active threads, and
a list of *resigned threads* that experienced precision loss or ran out of fuel.

The active threads are executed using a simple round-robin schedule:

* If a thread terminates with value `Some c`, in which case all the other threads are discarded and `c` is evaluated.
* If a thread terminates with value `None`, it is discarded.
* If a thread resings, it is placed onto the list of resigned threads.

Once the queue of active threads becomes empty:

* If there are any resigned threads, the operation `Resign` is performed. Upon resumption, all the resigned threads are resumed (with better precision and more fuel).
* If there are no resigned threads, the error `InvalidCase` is reported.


