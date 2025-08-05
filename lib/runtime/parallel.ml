(** Parallel evaluation for Clerical expressions. *)

(* Raised if all guards evaluate to [false]. *)
exception InvalidCase

(** Yield due to precision loss or spent loop fuel. Upon resumption, either
    restart with better precision, or resume with more fuel. *)
let yield = Picos_std_structured.Control.yield

let rec extract_exn = function
  | Picos_std_structured.Control.Errors [(exn, _); _] -> extract_exn exn
  | Picos_std_structured.Control.Errors [] -> assert false
  | exn -> exn

(** Run guards in parallel and return the result of the first one that succeeds.
*)
let run_guards guards =
  (* It looks like here it may happen that serveral guards raise an exception
     simultaneously and then Picos collects them all into a single exception that
     carries the list of all exceptions that happened. We catch it and just
     throw the first exception in the list. TODO: Is this a reasonable policy? *)
  try
    Picos_std_structured.Run.first_or_terminate
    @@ List.map
      (fun (g, v) () ->
         if g () then v else raise Picos_std_structured.Control.Terminate)
      guards
  with
  | Picos_std_structured.Control.Errors [(exn, _); _] -> raise (extract_exn exn)

(** Run a toplevel computation that sets up the domains. *)
let toplevel ?domains task =
  let n_domains =
    match domains with
    | None -> Picos_domain.recommended_domain_count () - 1
    | Some k -> k
  in
  try Picos_mux_multififo.run_on ~heartbeat_delay:0.05 ~n_domains @@ task
  with Picos_std_structured.Control.Terminate -> raise InvalidCase

(** Map a function on a list in parallel *)
let map f xs =
  try
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
  with
  | Picos_std_structured.Control.Errors [(exn, _); _] -> raise (extract_exn exn)

let array_map f xs =
  try
    Picos_std_structured.Bundle.join_after ~on_return:`Terminate @@ fun bundle ->
    (* Create a list of promises that immediately start running *)
    let ps =
      Array.map
        (fun x ->
           Picos_std_structured.Bundle.fork_as_promise bundle (fun () -> f x))
        xs
    in
    (* Await them all and return the results *)
    let ys = Array.map Picos_std_structured.Promise.await ps in
    (* Tell all the promises to go away. *)
    Array.iter (fun p -> Picos_std_structured.Promise.terminate p) ps;
    ys
  with
  | Picos_std_structured.Control.Errors [(exn, _); _] -> raise (extract_exn exn)
