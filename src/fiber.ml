(** Custom-made cooperative fibers for evaluating Clerical expressions. *)

module Make(R : sig type t end) =
struct

open Effect.Deep

type fiber = (unit, R.t) continuation

(** A fiber may yield, resign, or abort. *)
type _ Effect.t +=
   | Fork : (unit -> R.t) -> unit Effect.t
   | Yield : unit Effect.t (* Yield to another fiber *)
   | Resign : unit Effect.t (* Stop execution due to precission loss or spent fuel *)

exception Abort (* Abort execution without possibility of resumption *)

(** Lipstick *)
let return x = x

(** Fork a fiber. *)
let fork f = Effect.perform (Fork f)

(** Yield to another fiber. *)
let yield () = Effect.perform Yield

(** Give up due to precision loss or spent loop fuel.
    Upon resumption, either restart with better precision, or resume with more fuel.
 *)
let resign () = Effect.perform Resign

(** Give up without possibility of resumption. *)
let abort () = raise Abort

let run_fibers (fibers : (unit -> R.t) list) : R.t =

  (* the queue of active fibers *)
  let active : fiber Queue.t = Queue.create () in

  (* the list of resigned fibers *)
  let resigned : fiber list ref = ref [] in

  (* enqueue a continuation as a fiber *)
  let enqueue (k : fiber) =
    Queue.push k active
  in

  let shelf (k : fiber) =
    resigned := k :: !resigned
  in

  let unshelf_all () =
    (* Make resigned fibers active. *)
    List.iter (fun k -> Queue.add k active) !resigned ;
    (* Clear the queue. *)
    resigned := []
  in

  (** Properly dispose of all fibers *)
  let discontinue_fibers () =
    Queue.iter (fun k -> ignore (discontinue k Abort)) active ;
    List.iter (fun k -> ignore (discontinue k Abort)) !resigned
  in

  (** Dequeue a fiber and activate it, if there is one. *)
  let dequeue () : R.t =
    match Queue.take_opt active with
    | Some k -> continue k ()
    | None ->
       begin match !resigned with
       | [] -> raise Abort
       | _::_ ->
          (* Wait until we're told to restart the resigned fibers. *)
          try
            resign () ;
            unshelf_all () ;
            continue (Queue.pop active) ()
          with
          | Abort -> discontinue_fibers () ; raise Abort
          end
  in

  (* Run fibers, take the result of the first one that terminates. *)
  let rec run main =
    match_with
      main
      ()
      { retc = (fun v -> discontinue_fibers () ; v)
      ; exnc = (function Abort -> dequeue () | exc -> raise exc)
      ; effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Fork f ->Some (fun (k : (a, R.t) continuation) -> enqueue k ; run f)
        | Yield -> Some (fun (k : (a, R.t) continuation) -> enqueue k ; dequeue ())
        | Resign -> Some (fun (k : (a, R.t) continuation) -> shelf k ; dequeue ())
        | _ -> None
      )
      }
  in

  run (fun () -> List.iter fork fibers ; abort ())

let defibrillator =
  { retc = (fun v -> v)
  ; exnc = (fun ex -> raise ex)
  ; effc = (fun (type a) (eff : a Effect.t) ->
    match eff with
    | Yield -> Some (fun (k : (a, 'b) continuation) -> continue k ())
    | Resign -> Some (fun (k : (a, 'b) continuation) -> continue k ())
    | _ -> None
  )
  }

end
