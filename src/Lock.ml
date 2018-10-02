(* A lock is implemented as a reference to a Boolean flag. *)

type t =
  bool ref (* [true] if taken *)

type lock =
  t

let create () =
  (* A lock is created free. *)
  ref false

let acquire l =
  if !l then
    (* Fail if the lock is taken. *)
    assert false
  else
    (* Mark the lock as taken. *)
    l := true

let release l =
  if not !l then
    (* Fail if the lock is free. *)
    assert false
  else
    (* Mark the lock as free. *)
    l := false

let holding l f x =
  acquire l;
  let y = f x in
  release l;
  y
    (* We assume that [f] cannot raise an exception, or raises only fatal
       exceptions, so there is no need to unlock when an exception is raised. *)

let nop () =
  ()

let touch l =
  holding l nop ()
    (* This operation can be optimised as: [acquire l; release l]    *)
    (*           and further optimised as: [if !l then assert false] *)
