# Clerical syntax

This file describes the syntax of Clerical. It is hopefully up to date. If you supsect
that it is not, you will have to consult `src/parser.mly`, sorry.

## Toplevel commands

These are the commands you can put in a `.real` file or type into the interactive top level.

### Datatypes

The datatypes are

* `bool` – booleans `true` and `false`
* `int` – integers (implemented using GMP large integers)
* `real` – real numbers (using MPFR for arbitrary precision)

### Function definition

A function is defined by

    function ⟨functionName⟩ (⟨ty1⟩ ⟨arg1⟩, ..., ⟨tyN⟩ ⟨argN⟩):
       ⟨body⟩

The return type of `⟨functionName⟩` is computed automatically. Example:

    function power(real x, int k):
       var i := 0
       and p := 1.0 in
         while i < k do
           p := p *. x ;
           i := i + 1
         end ;
         p

Functions may *not* be recursive, but they may refer to previously defined functions.

### External declarations

To extend Clerical with a new primitive function, define its OCaml implementation in `src/external.ml`. In Clerical you can then bind the OCaml definition to Clerical with

    external ⟨funcName⟩ : (⟨ty1⟩, .., ⟨tyN⟩) -> ⟨ty_ret⟩ = "⟨str⟩"

where:

* `⟨funcName⟩` is the name of the function in Clerical (you may use infix operators if
you put them in parentheses),
* `⟨ty1⟩`, .., `⟨tyN⟩` are types of arguments,
* `⟨ty_ret⟩` is the return type,
* `⟨str⟩` is the string identifier of the OCaml definition.

Please consult `src/external.ml` and `prelude.real` for examples.


### Runinng a command

The toplevel command

    do ⟨cmd⟩

evaluates command `⟨cmd⟩`. For example:

    do 1.0 +. 3.0

### Setting precision

You can set the *initial* precision at which a toplevel command is computed with

    precision ⟨int⟩

where `⟨int⟩` is the precision at which MPFR will compute. It has to be at least 2.

### Loading a file

You may load a file into Clerical with

    load "⟨fileName⟩"

Alternatively, you can load it on the command line with `-l` option. Run Clerical with
`--help` to see other command-line options.

## Syntax of expressions

### Variables

Variable names are case-sensitive. They have to start with a letter or underscore `_`.
They may contain digits, and they may end in any number of `'`.

### Constants

* Boolean constants: `true`, `false`
* Integer constants are written in decimal
* Real constants are written in floating-point format.

Note that `42` is always an integer, and `42.0` is always a real. There is no automatic
conversion from integers to reals.

### `skip`

Skip does not do anything.

### `c₁ ; c₂`

Do `c₁` then do `c₂`.

### Non-deterministic case

A non-deterministic case takes the form

    case
     | b₁ => c₁
     | b₂ => c₂
     ...
     | bᵢ => cᵢ
    end

It executes one of `ci` for which `bi` is true.

### Conditional statement

The conditional is written as

    if b then c₁ else c₂

The statement must have both the `then` and the `else` branches.

### `while` loop

A `while` loop is written as

    while b do
      c
    done

### Local definitions (read-only)

Local definitions are introduced as

    let x₁ = e₁
    and x₂ = e₂
    ...
    and xᵢ = eᵢ in
      c

The definitions are valid in `c`.

### Local variables (mutable)

Local variables are introduced as

    var x₁ := e₁
    and x₂ := e₂
    ...
    and xᵢ := eᵢ in
      c

The variables are valid in `c`.

### Assignment

A variable is assigned to as

    x := e

### Limits

A limit of a sequence is defined as

    lim n => e

where `e` must evaluate to a term which is within distance `2⁻ⁿ` of `n`.

### Function application

A function is applied as

    f(e₁, ..., eᵢ)

Function names may be valid OCaml infix operators. These can be written in infix notation,
or as function calls, but then their names have to be parenthesized. Example:

    do 1 + 2
    do (+)(1,2)

The function `(+)` is defined in `prelude.real`.

### Arithmetical operations & comparisons

Consult `prelude.real` for an up-to-date list of supported operators.

Because Clerical has no subtyping and no polymorphism, we cannot use the same operator for
two functions. Consequently, we follow the following convention: infix operations such as
`+`, `-`, `*` act on integers. To have them act on reals, place a period after them, e.g.,
`+.`, `-.`, `*.`. (Yes, this is annoying.)



