
(** Radix sort.

    This module implements several versions of radix sort,
    following Sedgewick & Wayne's Algorithms, 4th edition. *)

module Strings : sig

  val lsd: string array -> int -> unit
  (** [lsd a w] sorts the array [a] in increasing order according to
      the first [w] characters of each string.
      Raises [Invalid_argument "lsd"] if one string has less than [w]
      characters.
      Uses O(wN) time and O(N) space where N is the length of [a].
      Stable.

      Experiments show that it is faster than [Array.sort] for values of
      [w] smaller than 5, but slower otherwise. *)

  val msd: string array -> unit
  (** [msd a] sorts the array [a] in increasing order.
      Uses O(Nlog(N)) time and O(N) space where N is the length of [a].
      Stable.

      No better than [Array.sort], so of little interest. *)

end

module Ints : sig

  val sort: int array -> unit
  (** [sort a] sorts the array [a] in increasing order.
      Uses O(N) time and O(N) space where N is the length of [a].

      Experiments show that it is roughly three times faster than
      [Array.sort]. *)

end
