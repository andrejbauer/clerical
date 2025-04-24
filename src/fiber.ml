(** Cooperative fibers for evaluating Clerical expressions. *)
module Make (R : sig
  type t
end) =
struct
  open Effect.Deep

  (** Lipstick *)
  let return x = x

  (** Yield due to precision loss or spent loop fuel. Upon resumption, either
      restart with better precision, or resume with more fuel. *)
  let yield () = Eio.Fiber.yield ()

  let cancel () = Eio.Fiber.await_cancel ()

  let run_fibers ~pool ~weight (fibers : (unit -> R.t) list) : R.t =
    let task_wrap task =
      Eio.Executor_pool.submit_exn pool ~weight (fun () -> task ())
    in
    let rec make_jobs = function
      | [] -> []
      | head :: tail -> (fun () -> task_wrap head) :: make_jobs tail
    in
    Eio.Fiber.any (make_jobs fibers)
end
