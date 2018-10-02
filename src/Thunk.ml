open Lock

type 'a state =
| Evaluated of      (* result: *)          'a
| Suspended of (* computation: *) (unit -> 'a)

(* Both [lock] and [state] are references. One might think that it
   would be more efficient to flatten everything and make this a
   record with two mutable fields. This is true, but 1- that would
   make the proof more difficult / less modular and 2- we do not
   care, since this is just a model anyway. In practice, we will
   use the primitive suspensions offered by OCaml's [Lazy] module. *)

type 'a t =
  lock * 'a state ref

type 'a thunk =
  'a t

let create (f : unit -> 'a) : 'a thunk =
  (* Initializing the state before creating the lock means that we
     can get away with a simpler proof rule, where the lock invariant
     must be established before the lock is created. *)
  let s = ref (Suspended f) in
  let l = Lock.create() in
  l, s

let constant (x : 'a) : 'a thunk =
  let s = ref (Evaluated x) in
  let l = Lock.create() in
  l, s

let force ((lock, s) : 'a thunk) : 'a =
  Lock.holding lock (fun () ->
    match !s with
    | Evaluated x ->
        x
    | Suspended f ->
        (* We hold the lock while the call to [f] is in progress. As a
           result, if this call attempts to re-entrantly force this
           thunk, the re-entrant call will fail. This is the desired
           behavior. If we released the call before calling [f], we
           would not be able to enforce that [f] be called at most once. *)
        let x = f() in
        s := Evaluated x;
        x
  ) ()

let pay ((lock, _) : 'a thunk) : unit =
  Lock.touch lock
    (* TEMPORARY ideally, no operation at all should suffice *)

(* TEMPORARY If [f] raises an exception, then the above code will leave
   the thunk in a locked state, so any attempt to force it again will fail. *)

let recursive (f : 'a thunk -> 'a thunk) : 'a thunk =
  (* Create an uninitialized thunk. *)
  let dummy () = assert false in (* cannot fail, really *)
  let s = ref (Suspended dummy) in
  let t : 'a thunk = Lock.create(), s in
  (* Overwrite [s] with a suspended computation that applies [f] to this
     thunk. If [f] misbehaves and tries to force the thunk or otherwise build a
     nonproductive data structure, a failure will occur, not in the function
     [dummy] above, but while trying to take an already taken lock. *)
  s := Suspended (fun () -> force (f t));
  t

