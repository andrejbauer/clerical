(** Parallel evaluation for Clerical expressions. *)

(* Raised if all guards evaluate to [false]. *)
exception InvalidCase

(** Yield due to precision loss or spent loop fuel. Upon resumption, either
    restart with better precision, or resume with more fuel. *)
let yield = Picos_std_structured.Control.yield

(** Run guards in parallel and return the result of the first one that succeeds.
*)
let run_guards guards =
  Picos_std_structured.Run.first_or_terminate
  @@ List.map
       (fun (g, v) () ->
         if g () then v else raise Picos_std_structured.Control.Terminate)
       guards

(** Run a toplevel computation that sets up the domains. *)
let toplevel ?domains task =
  let n_domains =
    match domains with
    | None -> Picos_domain.recommended_domain_count () - 1
    | Some k -> k
  in
  try Picos_mux_multififo.run_on ~n_domains @@ task
  with Picos_std_structured.Control.Terminate -> raise InvalidCase
