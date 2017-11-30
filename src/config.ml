(** Configuration parameters *)

type prelude =
  | PreludeNone
  | PreludeDefault
  | PreludeFile of string

let prelude_file = ref PreludeDefault

let interactive_shell = ref true

let wrapper = ref (Some ["rlwrap"; "ledit"])

let max_boxes = ref 42

let columns = ref (Format.get_margin ())

let init_prec = ref 16

let max_prec = ref 2048

let trace = ref false
