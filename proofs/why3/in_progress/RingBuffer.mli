type 'a buffer
(*@ mutable model sequence: 'a seq  *)
(*@         model capacity: integer *)

val create: int -> dummy:'a -> 'a buffer
(*@ b = create n ~dummy
      requires 0 < n <= Sys.max_array_length
      ensures  b.capacity = n
      ensures  b.sequence = empty *)

val length: 'a buffer -> int
(*@ n = length b
      ensures n = length b.sequence *)

val clear: 'a buffer -> unit
(*@ clear b
      modifies b
      ensures  b.sequence = empty *)

val push: 'a buffer -> elt:'a -> unit
(*@ push b ~elt
      requires length b.sequence < b.capacity
      modifies b
      ensures  length b.sequence = length (old b.sequence) + 1
      ensures  b.sequence = old b.sequence ++ (Seq.cons elt empty) *)

val peek: 'a buffer -> 'a
(*@ r = peek b
      requires length b.sequence > 0
      ensures  r = b.sequence[0] *)

val pop: 'a buffer -> 'a
(*@ r = pop b
      requires length b.sequence > 0
      modifies b
      ensures  length b.sequence = length (old b.sequence) - 1
      ensures  r = (old b.sequence)[0]
      ensures  old b.sequence = Seq.cons r (old b.sequence) *)
