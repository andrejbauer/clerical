open Eio.Std
(** Clerial main program *)

(** The usage message. *)
let usage = "Usage: clerical [option] ... [file] ..."

(** A list of files to be loaded and run, together with information on whether
    they should be loaded in interactive mode. *)
let files = ref []

(** Add a file to the list of files to be loaded, and record whether it should
    be processed in interactive mode. *)
let add_file quiet filename = files := (filename, quiet) :: !files

(** Command-line options *)
let options =
  Arg.align
    [
      ( "--columns",
        Arg.Set_int Config.columns,
        " Set the maximum number of columns of pretty printing" );
      ( "--no-prelude",
        Arg.Unit (fun () -> Config.prelude_file := Config.PreludeNone),
        " Do not load the prelude.m31 file" );
      ( "--prelude",
        Arg.String (fun str -> Config.prelude_file := Config.PreludeFile str),
        "<file> Specify the prelude file to load initially" );
      ( "-v",
        Arg.Unit
          (fun () ->
            Format.printf "Andromeda %s (%s)@." Build.version Sys.os_type;
            exit 0),
        " Print version information and exit" );
      ( "-n",
        Arg.Clear Config.interactive_shell,
        " Do not run the interactive toplevel" );
      ( "-l",
        Arg.String (fun str -> add_file true str),
        "<file> Load <file> into the initial environment" );
      ( "--init-prec",
        Arg.Set_int Config.init_prec,
        "<int> Set initial precision for MFPR" );
      ( "--max-prec",
        Arg.Set_int Config.max_prec,
        "<int> Set maximum precision for MPFR" );
      ( "--out-prec",
        Arg.Set_int Config.out_prec,
        "<int> Set precision for printing reals at top level" );
      ( "--trace",
        Arg.Set Config.trace,
        " Print trace information during evaluation" );
      ( "--verbose",
        Arg.Set Config.verbose,
        " Print information about precision during computation" );
    ]

(** Interactive toplevel *)
let interactive_shell ~pool state =
  Format.printf "Clerical %s@." Build.version;

  let rec loop state =
    let state =
      try Toplevel.exec_interactive ~pool state with
      | Ulexbuf.Error { Location.data = err; Location.loc } ->
          Print.message ~loc "Syntax error" "%t" (Ulexbuf.print_error err);
          state
      | Desugar.Error { Location.data = err; Location.loc } ->
          Print.message ~loc "Syntax error" "%t" (Desugar.print_error err);
          state
      | Typecheck.Error { Location.data = err; Location.loc } ->
          Print.message ~loc "Type error" "%t" (Typecheck.print_error err);
          state
      | Runtime.Error { Location.data = err; Location.loc } ->
          Print.message ~loc "Runtime error" "%t" (Runtime.print_error err);
          state
      | Sys.Break ->
          Print.message ~loc:Location.Nowhere "Runtime" "interrupted";
          state
    in
    loop state
  in
  try loop state with End_of_file -> ()

(** Main program *)
let _main ~pool =
  Sys.catch_break true;
  (* Parse the arguments. *)
  Arg.parse options
    (fun str ->
      add_file false str;
      Config.interactive_shell := false)
    usage;
  (* Files were accumulated in the wrong order, so we reverse them *)
  files := List.rev !files;
  (* Should we load the prelude file? *)
  (match !Config.prelude_file with
  | Config.PreludeNone -> ()
  | Config.PreludeFile f -> add_file true f
  | Config.PreludeDefault ->
      (* look for prelude next to the executable and don't whine if it is not there *)
      let d = Filename.dirname Sys.argv.(0) in
      let f = Filename.concat d "prelude.real" in
      if Sys.file_exists f then add_file true f);

  (* Set the maximum depth of pretty-printing, after which it prints ellipsis. *)
  Format.set_max_boxes !Config.max_boxes;
  Format.set_margin !Config.columns;
  Format.set_ellipsis_text "...";
  try
    (* Run and load all the specified files. *)
    let topstate =
      List.fold_left
        (fun topstate (fn, quiet) ->
          Toplevel.load_file ~pool ~quiet topstate fn)
        Toplevel.initial !files
    in

    if !Config.interactive_shell then interactive_shell ~pool topstate else ()
  with
  | Ulexbuf.Error { Location.data = err; Location.loc } ->
      Print.message ~loc "Syntax error" "%t" (Ulexbuf.print_error err)
  | Desugar.Error { Location.data = err; Location.loc } ->
      Print.message ~loc "Syntax error" "%t" (Desugar.print_error err)
  | Typecheck.Error { Location.data = err; Location.loc } ->
      Print.message ~loc "Type error" "%t" (Typecheck.print_error err)
  | Runtime.Error { Location.data = err; Location.loc } ->
      Print.message ~loc "Runtime error" "%t" (Runtime.print_error err)

let domains = Domain.recommended_domain_count ()

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let dm = Eio.Stdenv.domain_mgr env in
  _main ~pool:(Eio.Executor_pool.create ~sw ~domain_count:domains dm)
