(* This is a naive re-implementation of thunks on top of native
   references and locks. This code is not meant to be shipped
   with Vocal. It is used to reason about thunks in CFML and
   prove that CFML's axiomatization of thunks is sound. *)

(* The type of thunks. *)

type 'a t

type 'a thunk =
  'a t

(* Creation. *)

val create: (unit -> 'a) -> 'a thunk
val constant:        'a  -> 'a thunk
val recursive: ('a thunk -> 'a thunk) -> 'a thunk

(* Evaluation. *)

val force:      'a thunk -> 'a

(* The following function does nothing at runtime. *)

val pay: 'a thunk -> unit
