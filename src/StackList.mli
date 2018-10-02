
type 'a t

val create: unit -> 'a t

include SequenceSig.ClearLength with type 'a t := 'a t

include StackSig.PushPopTop with type 'a t := 'a t

include StackSig.PopTopExnOpt with type 'a t := 'a t

include SequenceSig.IterMap with type 'a t := 'a t

include SequenceSig.Fold with type 'a t := 'a t
  (** [fold f acc stack] iterates from the top of the stack *)

