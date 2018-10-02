(* A cascade is a finite or infinite sequence of elements, which are produced on
   demand. The elements may be computed either ahead of time or on demand, with
   or without memoization; consumers have no control over this decision, which
   is made by producers. *)

(* TEMPORARY introduce terminology: persistent cascade (recomputed or memoized)
   versus ephemeral cascade (can be forced at most once) *)

type 'a head =
| Nil
| Cons of 'a * (unit -> 'a head)

type 'a cascade =
  unit -> 'a head

(* A producer is a function of a cascade to a cascade, which concatenates
   elements in front of its argument, without ever inspecting it. That is,
   if [xs] is a producer, then [concat (xs nil) ys] should be equivalent to
   [xs ys]. Producers are composable at no cost, whereas cascade
   concatenation is expensive. Producers form a monad whose unit is [cons]
   and whose sequencing operation is [bind]. *)

type 'a producer =
  'a cascade -> 'a cascade

(* A producer is converted to a cascade by applying it to the empty cascade.
   [run xs] is defined as [xs nil]. *)

val run: 'a producer -> 'a cascade

(* Conversely, a cascade [xs] is converted to a producer by [concat xs]. This
   should be avoided insofar as possible, because [concat] is inefficient. *)

(* -------------------------------------------------------------------------- *)

(* Constructors. *)

(* [nil] is the empty cascade. *)

val nil: 'a cascade

(* [cons] is the unit of the producer monad. That is, [cons x] is a producer
   which produces the element [x]. In other words, if [xs] is a cascade, then
   [cons x xs] is a cascade which begins with the element [x] and continues
   with the cascade [xs]. Note that [cons] is a function, hence is strict: in
   the expression [cons x xs], both [x] and [xs] are computed before the
   function [cons] is called. *)

val cons: 'a -> 'a producer

(* [bind] is the sequencing operation of the producer monad. *)

val bind: 'a producer -> ('a -> 'b producer) -> 'b producer

(* [singleton x] is the one-element cascade of just the element [x]. *)

val singleton: 'a -> 'a cascade

(* -------------------------------------------------------------------------- *)

(* Destructors. *)

(* [force xs] forces the production of the first element of the cascade [xs],
   if there is one. If the cascade [xs] is ephemeral, then it cannot be forced
   again. If it is persistent, then it can later be queried again. *)

val force: 'a cascade -> 'a head

(* The destructors that follow are simple variants of [force]. *)

(* [is_empty xs] forces the production of the first element of the cascade
   [xs], if there is one, and returns a Boolean result that indicates whether
   the cascade is empty. *)

val is_empty: 'a cascade -> bool

(* [head xs] forces the production of the first element of the cascade [xs]
   and returns it. It is an error to invoke this function if the cascade is
   empty. [head_opt xs] is analogous, but returns an option, and can be
   applied to an empty cascade. *)

val head:     'a cascade -> 'a
val head_opt: 'a cascade -> 'a option

(* [tail xs] forces the production of the first element of the cascade [xs]
   and returns its tail. It is an error to invoke this function if the cascade
   is empty. [tail_opt xs] is analogous, but returns an option, and can be
   applied to an empty cascade. *)

val tail:     'a cascade -> 'a cascade
val tail_opt: 'a cascade -> 'a cascade option

(* [drop n xs] is the cascade [xs], deprived of its [n] first elements.
   No side effect takes place when [drop n xs] is invoked, but [n+1]
   elements of [xs] are demanded when [drop n xs] is forced. *)

val drop: int -> 'a cascade -> 'a cascade

(* [get n xs] returns the [n]-th element of the cascade [xs], counting from 0
   and up. It is an error to invoke this function if the cascade has fewer
   than [n+1] elements. The time complexity of this function is linear.
   [get_opt n xs] is analogous, but returns an option, and can be applied to a
   cascade of [n] or fewer elements. *)

val get:     int -> 'a cascade -> 'a
val get_opt: int -> 'a cascade -> 'a option

(* The following functions exhaust their argument, which must be finite. *)

(* [fold_left f s xs] folds (iterates) the function [f] left-to-right over the
   cascade [xs], with initial state [s]. *)

val fold_left: ('accu -> 'a -> 'accu) -> 'accu -> 'a cascade -> 'accu

(* [iter f xs] iterates the function [f] left-to-right over the cascade [xs]. *)

val iter: ('a -> unit) -> 'a cascade -> unit

(* [length xs] is the length of the cascade [xs]. *)

val length: 'a cascade -> int

(* [to_list_rev xs] is the list of the elements of the cascade [xs], in reverse
   order. [to_list xs] is the list of the elements of the cascade [xs], in order. *)

val to_list_rev: 'a cascade -> 'a list
val to_list:     'a cascade -> 'a list

(* [rev xs] is the reverse of [xs]. No side effect takes place when [rev xs]
   is invoked. When it is forced, the whole cascade [xs] is forced, and a data
   structure of linear size is allocated in the heap. *)

val rev: 'a cascade -> 'a producer

(* [last xs] is the last element of the cascade [xs]. It is an error to invoke
   this function if the cascade is empty. [last_opt xs] is analogous, but
   returns an option, and can be applied to an empty cascade. [last1 x xs] is
   analogous, but returns [x] if the cascade [xs] is empty. *)

val last: 'a cascade -> 'a
val last_opt: 'a cascade -> 'a option
val last1: 'a -> 'a cascade -> 'a

(* [to_subarray xs a i j] writes the elements of the cascade [xs] to the array [a],
   beginning at index [i] included and going up to index [j] excluded. It returns
   a pair of the first index that was not written and the remainder of the cascade. *)

val to_subarray: 'a cascade -> 'a array -> int -> int -> int * 'a cascade

(* [to_array xs a] writes the elements of the cascade [xs] to the array [a].
   It returns a pair of the first index that was not written and the remainder
   of the cascade. *)

val to_array:    'a cascade -> 'a array               -> int * 'a cascade

(* [to_subbytes xs b i j] writes the elements of the cascade [xs] to the bytes
   buffer [b], beginning at index [i] included and going up to index [j]
   excluded. It returns a pair of the first index that was not written and the
   remainder of the cascade. *)
(* TEMPORARY CFML does not support:
val to_subbytes: char cascade -> bytes -> int -> int -> int * char cascade

(* [to_bytes xs b i j] writes the elements of the cascade [xs] to the bytes
   buffer [b]. It returns a pair of the first index that was not written and
   the remainder of the cascade. *)

val to_bytes:    char cascade -> bytes               -> int * char cascade
 *)
(* [to_channel c xs] writes the cascade [xs] to the output channel [c]. It
   returns the number of characters written, that is, the length of [xs]. *)
(* TEMPORARY CFML does not support:
val to_channel: out_channel -> char cascade -> int
 *)

(* [any f xs] is [true] if and only if some element of [xs] satisfies [f].
   [all f xs] is [true] if and only if all elements of [xs] satisfy [f]. *)

val any: ('a -> bool) -> 'a cascade -> bool
val all: ('a -> bool) -> 'a cascade -> bool

(* [find f xs] returns [Some x] if [x] is the first element of [xs] that
   satisfies [f]. It returns [None] if no such element exists. *)

val find: ('a -> bool) -> 'a cascade -> 'a option

(* [equal eq xs ys] is [true] if the cascades [xs] and [ys] are pointwise
   equal. The function [eq] is used to compare elements. *)

val equal: ('a -> 'b -> bool) -> 'a cascade -> 'b cascade -> bool

(* -------------------------------------------------------------------------- *)

(* Producers. *)

(* [identity] is the empty producer. [identity xs] is [xs]. *)

val identity: 'a producer

(* The cascade [concat xs ys] is the concatenation of the cascades [xs] and
   [ys]. The use of [concat] should be avoided, as its complexity is linear
   in the length of [xs]. One can often avoid it by chaining producers. *)

val concat: 'a cascade -> 'a producer

(* [take n xs] is the longest prefix of [xs] whose length is at most [n].
   Thus, if the length of [xs] is at most [n], then [take n xs] represents
   the sequence [xs]. Otherwise, [take n xs] is the sequence of the [n]
   first elements of [xs]. *)

val take: int -> 'a cascade -> 'a producer

(* [unfold] is the primitive corecursor, which means that it can in theory
   be used to build every cascade. The producer [unfold f s] produces the
   sequence of elements obtained by successively applying [f] to [s]. *)

val unfold: ('s -> ('a * 's) option) -> 's -> 'a producer

(* [replicate n x] is a sequence of [n] copies of [x]. *)

val replicate: int -> 'a -> 'a producer

(* The producer [from_option ox] produces zero elements if [ox] is [None] and
   produces the element [x] if [ox] is [Some x]. *)

val from_option: 'a option -> 'a producer

(* The producer [from_list xs] produces the elements of the list [xs]. *)

val from_list: 'a list -> 'a producer

(* The producer [from_iterator it] transmits the elements produced by the
   iterator [it]. An iterator is a function that returns a new element every
   time it is invoked, and returns [None] if there are no more elements.
   [from_iterator] can be viewed as a simplified version of [unfold] where
   ['s] is [unit]. *)

val from_iterator: (unit -> 'a option) -> 'a producer

(* TEMPORARY CFML does not support:
val from_substring: string -> int -> int -> char producer
val from_string: string -> char producer
val from_file: in_channel -> char producer
 *)

(* The producer [from_subarray a i j] produces the elements found in the
   array [a] between the indices [i] included and [j] excluded. *)

val from_subarray: 'a array -> int -> int -> 'a producer

(* The producer [from_array a] produces the elements of the array [a]. *)

val from_array: 'a array -> 'a producer

(* The producer [up i j] produces the integers between [i] included and
   [j] excluded, counting up. *)

val up: int -> int -> int producer

(* The producer [down i j] produces the integers between [i] excluded and
   [j] included, counting down. *)

val down: int -> int -> int producer

(* If [f] maps one element [x] to a sequence (represented by a producer), then
   [fold f xs] maps the cascade [xs] to the sequence obtained by concatenating
   (chaining) the sequences [f x] together. *)

val fold: ('a -> 'b producer) -> 'a cascade -> 'b producer

(* If [f] maps one element [x] to one element, then [map f xs] maps the
   cascade [xs] to the sequence of the [f x]'s. It can be defined as
   [fold (cons . f)]. *)

val map: ('a -> 'b) -> 'a cascade -> 'b producer

(* [flatten] is [fold concat]. *)

val flatten: 'a cascade cascade -> 'a producer

(* [filter f xs] produces the sequence of elements of [xs] that satisfy [f]. *)

val filter: ('a -> bool) -> 'a cascade -> 'a producer

(* If [eq] implements an equality test, then [uniq eq xs] is the sequence
   [xs], deprived of adjacent duplicate elements. As a corollary, if the
   sequence [xs] is sorted according to an ordering that is compatible with
   [eq], then [uniq eq xs] is the sequence [xs], deprived of all duplicate
   elements. *)

val uniq: ('a -> 'a -> bool) -> 'a cascade -> 'a cascade

(* If [f] maps a state and an element to a new state, then [scanl f s xs] is
   the sequence of states obtained by iterating [f], beginning in state [s],
   over the sequence [xs]. *)

val scanl: ('s -> 'a -> 's) -> 's -> 'a cascade -> 's cascade

(* Merge sort. The input cascade, as well as every intermediate cascade, is
   consumed only once. The output cascade itself must be consumed at most
   once, or must be memoized, otherwise some of the work of sorting will be
   duplicated. *)

val sort: ('a -> 'a -> int) -> 'a cascade -> 'a cascade

(* -------------------------------------------------------------------------- *)

(* Infinite cascades. *)

(* [repeat x] is the infinite cascade which produces [x] forever. *)

val repeat: 'a -> 'a cascade

(* [ints i] is the infinite cascade of the integers, beginning at [i] and
   counting up. *)

val ints: int -> int cascade

(* [iterate f x] is the infinite cascade whose elements are [x], [f x],
   [f (f x)], and so on. [iterate1 f x] is the infinite cascade whose
   elements are [f x], [f (f x)], and so on. *)

val iterate1: ('a -> 'a) -> 'a -> 'a cascade
val iterate : ('a -> 'a) -> 'a -> 'a cascade

(* [cycle] turns a finite cascade [xs] into an infinite one, which is the
   infinite (circular, so to speak) repetition of [xs]. It [xs] is an
   infinite cascade, then [cycle xs] is equivalent to [xs]. *)

val cycle: 'a producer -> 'a cascade

(* The fixed point combinator [recursive] accepts a recursive definition of a
   cascade, represented by a function [f] of type ['a cascade -> 'a cascade],
   and constructs its fixed point. Although the function [f] has the type of a
   producer, it is not necessarily a producer, i.e., it does not necessarily
   just concatenate elements in front of its argument. It can do more
   complicated things, like interleave elements in its argument, etc. Of
   course it should have type (later cascade -> cascade), that is, it should
   be productive. *)

(* [f] should produce a persistent cascade, otherwise the code is most likely
   to be incorrect. In fact, [f] should produce a memoized cascade, otherwise
   the code is most likely to have wrong complexity. *)

(* TEMPORARY CFML does not support:
val recursive: ('a cascade -> 'a cascade) -> 'a cascade
 *)

(* -------------------------------------------------------------------------- *)

(* Joining. Two input cascades, one output cascade. *)

(* [zip xs ys] is a sequence of pairs of elements drawn from [xs] and [ys].
   Its length is the minimum of the lengths of [xs] and [ys]. *)

val zip:                         'a cascade -> 'b cascade -> ('a * 'b) producer

(* [zipWith] is analogous to [zip], but transforms the pair [(x, y)] on the
   fly. *)

val zipWith: ('a -> 'b -> 'c) -> 'a cascade -> 'b cascade -> 'c producer

(* If the sequences [xs] and [ys] are sorted as per the total order [cmp],
   then [merge cmp xs ys] is the sorted sequence obtained by merging them. *)

val merge: ('a -> 'a -> int) -> 'a cascade -> 'a cascade -> 'a cascade

(* The sequence [interleave xs ys] begins with the first element of [xs],
   continues with the first element of [ys], and so on. If one of [xs] and
   [ys] is exhausted, it continues with the rest of the other sequence. *)

val interleave: 'a cascade -> 'a cascade -> 'a cascade

(* -------------------------------------------------------------------------- *)

(* Fork. One input cascade, two output cascades. *)

(* [unzip] forces its argument [xys] twice. Thus, [xys] must be persistent.
   Ideally, [xys] should be memoized, or cheap to recompute. If [xys] is
   ephemeral, or costly to recompute, one should use [unzip_memoize]
   instead. It memoizes [xys] before unzipping it. *)

val unzip:         ('a * 'b) cascade -> 'a producer * 'b producer
(* TEMPORARY CFML does not support:
val unzip_memoize: ('a * 'b) cascade -> 'a producer * 'b producer
 *)

(* [dispatch] forces its argument [xys] twice. Thus, [xys] must be persistent.
   Ideally, [xys] should be memoized, or cheap to recompute. If [xys] is
   ephemeral, or costly to recompute, one should use [dispatch_memoize]
   instead. It memoizes [xys] before unzipping it. *)

type ('a, 'b) sum =
| Left of 'a
| Right of 'b

val dispatch: ('a, 'b) sum cascade -> 'a producer * 'b producer
(* TEMPORARY CFML does not support:
val dispatch_memoize: ('a, 'b) sum cascade -> 'a producer * 'b producer
 *)

(* [partition f xs] returns a pair of the sequence of elements of [xs] that
   satisfy [f] and the sequence of elements of [xs] that do not satisfy [f].
   The cascade [xs] is forced twice, and [f] is applied twice to every element
   of [xs]. *)

val partition: ('a -> bool) -> 'a cascade -> 'a producer * 'a producer
(* TEMPORARY could offer [partition_memoize] *)

(* -------------------------------------------------------------------------- *)

(* Strategies. *)

(* If [xs] is an arbitrary (possibly ephemeral) cascade, then [memoize xs] is
   a persistent and memoized producer of the same sequence of elements. *)

(* TEMPORARY CFML does not support:
val memoize: 'a cascade -> 'a producer
 *)

(* If [xs] is an arbitrary (probably ephemeral) cascade, then [once xs] is an
   ephemeral producer of the same sequence of elements. If it is forced more
   than once, a runtime error will occur. This may be useful for debugging
   purposes. Once the code is believed to be correct, [once xs] can be
   replaced with [concat xs], and [run (once xs)] can be replaced with
   [xs]. *)

val once: 'a cascade -> 'a producer

(* If [xs] is an arbitrary cascade, then [precompute xs] forces [xs] to
   immediately produce all of its elements, internally stores them in a list,
   and returns a persistent cascade, based on this list. *)

val precompute: 'a cascade -> 'a producer

(* -------------------------------------------------------------------------- *)

(* Bit packing and unpacking. *)

(* TEMPORARY document *)

val pack:   int -> bool cascade -> int producer
val unpack: int -> int cascade -> bool producer
