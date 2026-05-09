# Clerical

An implementation of an imperative language for exact real number computation.

## Prerequisites

Clerical requires OCaml 5.0.0 or later (tested with 5.0.0 and 5.2.0) and
[opam](https://opam.ocaml.org).

The system-level (non-opam) dependency is the GNU multiple-precision
floating-point library [MPFR](http://www.mpfr.org). On macOS install it with
[Homebrew](https://brew.sh):

    brew install mpfr

(it pulls in GMP). On Debian/Ubuntu, `apt install libmpfr-dev libgmp-dev`.

The opam packages Clerical depends on directly are:

* [`dune`](https://dune.build) — build system
* [`menhir`](http://gallium.inria.fr/~fpottier/menhir/) — parser generator (provides the `menhirLib` runtime)
* [`sedlex`](https://github.com/ocaml-community/sedlex) — Unicode-aware lexer
* [`mlgmpidl`](https://github.com/nberth/mlgmpidl) — OCaml bindings to GMP and MPFR
* [`picos`](https://github.com/ocaml-multicore/picos), `picos_std`, `picos_mux` — structured concurrency

`clerical.opam` lists all of the above and pins the Picos packages to a specific
upstream commit (current released Picos does not yet expose the
`Run.first_or_terminate` primitive Clerical relies on). To install everything
in one shot, from the project root:

    opam install . --deps-only

If you prefer a project-local opam switch (recommended, keeps the toolchain
isolated to this checkout):

    opam switch create . 5.2.0
    eval $(opam env)
    opam install . --deps-only

## Compilation

To compile Clerical, run the following command in the Clerical directory:

    dune build

Dune compiles the program and creates the executable `clerical.exe`. You can
run it with:

    ./clerical.exe --prelude prelude.real

## Unit testing

Dune unit tests can be run with

    dune runtest

and validate tests with

    dune promote

See [Writing and running tests](https://dune.readthedocs.io/en/stable/tests.html)
section of the Dune documentation for further information.

## Repository structure

* `bin` – the `clerical` executable entry point
* `lib` – the OCaml implementation, split into `parsing`, `typing`, `reals`, `runtime`, `util`
* `examples` – examples of Clerical programs
* `doc` – documentation
* `prelude.real` – built-in functions and operators loaded by default

## Clerical syntax

Please consult:

* [`doc/syntax.md`](doc/syntax.md) for a brief explanation of the syntax
* [`examples`](./examples) for examples of Clerical programs
* [`prelude.real`](./prelude.real) for the built-in functions and operators

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
