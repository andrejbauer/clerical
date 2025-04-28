type prelude = PreludeNone | PreludeDefault | PreludeFile of string

val prelude_file : prelude ref
val interactive_shell : bool ref
val wrapper : string list option ref
val max_boxes : int ref
val columns : int ref
val init_prec : int ref
val max_prec : int ref
val out_prec : int ref
val trace : bool ref
val domains : int option ref
val verbose : bool ref
