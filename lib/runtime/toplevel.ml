module Desugar = Typing.Desugar
module Typecheck = Typing.Typecheck

type state = {
  desugar : Desugar.context;
  typecheck : Typecheck.context;
  runtime : Run.stack;
}
(** A toplevel computation carries around the current environment. *)

let initial =
  {
    desugar = Desugar.initial;
    typecheck = Typecheck.initial;
    runtime = Run.initial;
  }

let exec_interactive ~eio_ctx { desugar; typecheck; runtime } =
  let cmd = Parsing.Lexer.read_toplevel Parsing.Parser.commandline () in
  let desugar, cmd = Desugar.toplevel desugar cmd in
  let typecheck, cmd = Typecheck.toplevel typecheck cmd in
  let runtime = Eval.toplevel ~eio_ctx ~quiet:false runtime cmd in
  { desugar; typecheck; runtime }

let load_file ~eio_ctx ~quiet { desugar; typecheck; runtime } fn =
  let desugar, cmds = Desugar.load desugar fn in
  let typecheck, cmds = Typecheck.topfile typecheck cmds in
  let runtime = Eval.topfile ~eio_ctx ~quiet runtime cmds in
  { desugar; typecheck; runtime }
