
type 'a t

val create: dummy:'a -> 'a t

include SequenceSig.ClearLength with type 'a t := 'a t

include StackSig.PushPopTop with type 'a t := 'a t

(* we cannot reuse SequenceSig.IterMap since map has an extra argument dummy *)
(* include SequenceSig.IterMap with type 'a t := 'a t *)
val iter : ('a -> unit) -> 'a t -> unit
val map : dummy:'b -> 'a t -> ('a -> 'b) -> 'b t
val copy: 'a t -> 'a t

include SequenceSig.Fold with type 'a t := 'a t
  (** [fold f acc stack] iterates from the top of the stack *)

