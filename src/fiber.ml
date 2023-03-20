(** Custom-made cooperative fibers for evaluating Clerical expressions. *)

module Make(R : sig type t end) =
struct

open Effect.Deep

type fiber = (unit, R.t) continuation

(** A fiber may for a new fiber, yield or abort. *)
type _ Effect.t +=
   | Fork : (unit -> R.t) -> unit Effect.t
   | Yield : unit Effect.t (* Stop execution due to precission loss or spent fuel *)

exception Abort (* Abort execution without possibility of resumption *)

(** Lipstick *)
let return x = x

(** Fork a fiber. *)
let fork f = Effect.perform (Fork f)

(** Yield due to precision loss or spent loop fuel.
    Upon resumption, either restart with better precision, or resume with more fuel.
 *)
let yield () = Effect.perform Yield

(** Give up without possibility of resumption. *)
let abort () = raise Abort

let run_fibers (fibers : (unit -> R.t) list) : R.t =

  (** The queue of active fibers *)
  let active = Queue.create () in

  (** The queue of inactive fibers *)
  let inactive = Queue.create () in

  (** Enqueue a continuation as an active fiber. *)
  let enqueue (k : fiber) =
    Queue.push k active
  in

  (** Place an inactive fiber into the inactive queue. *)
  let shelf (k : fiber) =
    Queue.push k inactive
  in

  (** Make inactive fibers active. *)
  let unshelf_all () =
    while not (Queue.is_empty inactive) do
      Queue.add (Queue.take inactive) active
    done
  in

  (** Properly dispose of all fibers. *)
  let discontinue_all () =
    let to_list = Queue.fold (fun fs f -> f :: fs) [] in
    let lst = to_list inactive @ to_list active in
    Queue.clear inactive ;
    Queue.clear active ;
    List.iter (fun k -> try ignore (discontinue k Abort) with Abort -> ()) lst
  in

  (** Dequeue a fiber and activate it, if there is one. *)
  let dequeue () : R.t =
    if Queue.is_empty active then
      begin
        (* The active queue is empty, we restart the inactive fibers. *)
        unshelf_all () ;
        (* Before proceeding, we yield so that outer-level fibers can run.*)
        yield ()
      end ;
    (* Pop a fiber off the queue and run it, or abort if none are available. *)
    match Queue.take_opt active with
    | Some k -> continue k ()
    | None -> abort ()
  in

  (* Run fibers, and take the result of the first one that terminates. *)
  let rec run main =
    match_with
      main
      ()
      { retc = (fun v -> discontinue_all () ; v)
      ; exnc = (function
                | Abort ->
                   (* if the fiber aborts we try another one *)
                   dequeue ()
                | exc ->
                   (* other exceptions propagate outwards *)
                   raise exc)
      ; effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Fork f ->Some (fun (k : (a, 'b) continuation) -> enqueue k ; run f)
        | Yield -> Some (fun (k : (a, 'b) continuation) -> shelf k ; dequeue ())
        | _ -> None
      )
      }
  in

  run (fun () -> List.iter fork fibers ; abort ())

(** Top-level handler that just restarts any fiber that yields.
    It does not support forking of new fibers. *)
let defibrillator =
  { retc = (fun v -> v)
  ; exnc = (fun ex -> raise ex)
  ; effc = (fun (type a) (eff : a Effect.t) ->
    match eff with
    | Yield -> Some (fun (k : (a, 'b) continuation) -> continue k ())
    | _ -> None
  )
  }

end
