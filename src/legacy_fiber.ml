(** Cooperative fibers for evaluating Clerical expressions. *)
module Make (R : sig
  type t
end) =
struct
  open Effect.Deep
  open Eio.Std

  exception Case_success of R.t

  type fiber = (unit, R.t) continuation

  (** A fiber may fork a new fiber, yield or abort. *)
  type _ Effect.t +=
    | Fork : (unit -> R.t) -> unit Effect.t
    | Yield :
        unit Effect.t (* Stop execution due to precission loss or spent fuel *)

  exception Abort (* Abort execution without possibility of resumption *)

  (** Lipstick *)
  let return x = x

  (** Fork a fiber. *)
  let fork f = Effect.perform (Fork f)

  (** Yield due to precision loss or spent loop fuel. Upon resumption, either
      restart with better precision, or resume with more fuel. *)
  let yield () = Effect.perform Yield

  (** Give up without possibility of resumption. *)
  let abort () = raise Abort

  let run_fibers ~pool (fibers : (unit -> unit) list) : R.t option =
    let task_wrap task =
      Eio.Executor_pool.submit_exn pool ~weight:0.0 (fun () -> task ())
    in
    let rec make_jobs = function
      | [] -> []
      | head :: tail -> (fun () -> task_wrap head) :: make_jobs tail
    in

    try
      Fiber.all (make_jobs fibers);
      None
    with Case_success c -> Some c

  (** Top-level handler that just restarts any fiber that yields. It does not
      support forking of new fibers. *)
  let defibrillator =
    {
      retc = (fun v -> v);
      exnc = (fun ex -> raise ex);
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Yield -> Some (fun (k : (a, 'b) continuation) -> continue k ())
          | _ -> None);
    }
end
