
module type PushPopTop = sig

  type 'a t

  val push: 'a t -> 'a -> unit
  (** [Vector.push a x] appends [x] at the end of vector [a], i.e.,
      increases the size of [a] by one and stores [x] at the rightmost
      position.

      Note: the order of the arguments is not that of {!Stack.push}. *)

  val pop: 'a t -> 'a
  (** [Vector.pop a] removes and returns the rightmost element in vector [a],
      or raises [Empty] if the stack is empty. *)

  val top: 'a t -> 'a
  (** [Vector.top a] returns the rightmost element in vector [a],
      or raises [Empty] if the vector is empty. *)

end

module type PopTopExnOpt = sig

  type 'a t

  exception Empty

  val pop_exn: 'a t -> 'a
  val top_exn: 'a t -> 'a

  val pop_opt: 'a t -> 'a option
  val top_opt: 'a t -> 'a option

end

module type S = sig
  include PushPopTop
  include SequenceSig.ClearLength with type 'a t := 'a t
end
