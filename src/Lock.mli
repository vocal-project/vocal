(* This is a simple implementation of locks in a sequential setting. *)

(* This code is currently not shipped with Vocal, although it could be,
   if we find a use for it. A lock can be useful, even in a sequential
   setting, to guard against re-entrant calls. *)

(* The type of locks. *)

type t

type lock =
  t

(* A lock is created free. *)

val create: unit -> lock

(* Acquiring a lock fails at runtime if the lock is already taken. *)

val acquire: lock -> unit

(* Releasing a lock is permitted if one holds it. *)

val release: lock -> unit

(* Touching a lock means acquiring the lock and releasing it immediately.
   This can be used to perform a ghost command while holding the lock. *)

val touch: lock -> unit

(* [holding l f x] executes the function call [f x] within a critical section
   for the lock [l]. The function [f] must not raise any exception. *)

val holding: lock -> ('a -> 'b) -> 'a -> 'b
