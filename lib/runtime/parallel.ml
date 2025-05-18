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

(** Map a function on a list in parallel *)
let map f xs =
  Picos_std_structured.Bundle.join_after ~on_return:`Terminate @@ fun bundle ->
  (* Create a list of promises that immediately start running *)
  let ps =
    List.map
      (fun x ->
        Picos_std_structured.Bundle.fork_as_promise bundle (fun () -> f x))
      xs
  in
  (* Await them all and return the results *)
  let ys = List.map Picos_std_structured.Promise.await ps in
  (* Tell all the promises to go away. *)
  List.iter (fun p -> Picos_std_structured.Promise.terminate p) ps;
  ys
