(* Let a wave be a (total) function of integer addresses to data. *)

(* A set of waves [ws] has period [p] if and only if membership in [ws] is
   invariant under shifting by [p]. This means, intuitively, that shifting a
   wave in [ws] by [p] towards the left or towards the right must yield a
   wave in [ws]. This does not imply that every wave [w] in [ws] is a
   periodic function. *)

(* [c] is a valid cutpoint in a set of waves [ws] iff stitching any two waves
   of [ws] at [c] yields a wave in [ws]. *)

module Make (V : sig

  (* A value [v] is an abstract code for a nonempty set of waves [ws].
     We say that [v] denotes [ws], or that the semantics of [v] is [ws]. *)

  type value

  (* The type [value] must be inhabited. The value [dummy] is used in
     a few places in dead code. It is irrelevant. *)

  val dummy : value

  (* The set of waves denoted by a value [v] must have a positive period,
     given by [period v]. This should ideally be, but does not absolutely have
     to be, the minimal period. *)

  val period : value -> int

  (* The set of waves denoted by a value [v] must have a valid cutpoint. More
     specifically,

     - If [origin v] is [Some c], then [c] must be a valid cutpoint, which means
       that [c + period v], [c - period v], and so on, are also valid cutpoints.

     - If [origin v] is [None], then every point must be a valid cutpoint. In that
       case, the value of [period v] is entirely irrelevant. *)

  val origin : value -> int option

  (* The test [same v1 v2] must indicate whether the values [v1] and [v2] are
     equal. This implies of course that the sets of waves denoted by [v1] and
     [v2] are equal. The converse need not be true. *)

  val same : value -> value -> bool

  (* The value [shift p v] must denote the same set of waves as [v], shifted
     by [p] towards the right. Shifting must interact well with the notion of
     equality implemented by the function [same] above: that is, the following
     laws must hold:

                       shift 0 v = v
               shift (p1 + p2) v = shift p1 (shift p2 v)

     Intuitively, these laws mean that shifting must not cause a loss of
     precision. Furthermore, shifting a value by [p] must leave its period
     unchanged, and must move its origin by [p] (modulo its period). That
     is, the following laws must hold:

              period (shift p v) = period v

           if origin v = None then:
              origin (shift p v) = None

           if origin v = Some c then, for some c':
              origin (shift p v) = Some c' &&
               c' mod (period v) = (p + c) mod (period v)

  *)

  val shift : int -> value -> value

  (* The test [subv max delta1 v1 delta2 v2] must indicate whether, on the
     closed interval of [0] to [max], the set of waves denoted by [shift
     delta1 v1] is a subset of the set of waves denoted by [shift delta2 v2].
     This test must be sound and complete, that is, it must return [true] if
     and only this set inclusion holds. *)

  val subv : int -> int -> value -> int -> value -> bool

  (* The value [join v1 v2] must denote an upper approximation of the join
     of [v1] and [v2]. That is, the set of waves denoted by [join v1 v2]
     must contain both the set of waves denoted by [v1] and that denoted
     by [v2]. *)

  val join : value -> value -> value

  (* If [lo1, hi1] and [lo2, hi2] are nonempty adjacent (closed) intervals,
     then [patch lo1 hi1 lo2 hi2 v1 v2] must be a value whose semantics on the
     interval [lo1, hi2] is (an upper approximation of) the concatenation of
     the semantics of [v1] on [lo1, hi1] and [v2] on [lo2, hi2]. *)

  (* (Used only when attempting to read across a boundary.) *)

  val patch : int -> int -> int -> value -> value -> value

end) : sig

  type value = V.value

  type lo     = int
  type hi     = int
  type base   = int
  type max    = int

  type amap

  val lo: amap -> lo
  val hi: amap -> hi

  type element =
    | E of base * max * value

  (* [zero lo] produces a map whose domain is the empty interval
     at [lo]. Same as [build lo (lo - 1) []]. *)
  val zero: base -> amap

  (* Precondition: [0 <= max].
     [singleton base max pat] is equivalent to
     [build base (base + max) [E base max pat]]. *)
  val singleton: base -> max -> value -> amap

  (* Precondition: [lo a1 + 1 = hi a2]. *)
  val concat: amap -> amap -> amap

  (* Precondition: [lo a1 = lo a2 && hi a1 = hi a2]. *)
  val merge: amap -> amap -> amap
  val sub: amap -> amap -> bool

  val concat_singleton_left : base -> max -> value -> amap -> amap
  val concat_singleton_right: amap -> base -> max -> value -> amap

  val keep_below: hi -> amap -> amap
  val keep_above: lo -> amap -> amap
  (* Precondition: [lolimit <= hilimit]. *)
  val keep_interval: lo -> hi -> amap -> amap

  type 'a cascade =
    unit -> 'a head

   and 'a head =
     | Nil
     | Cons of 'a * 'a cascade

  val elements : amap -> element cascade

  val elements_below : hi -> amap -> element cascade

  val elements_above : lo -> amap -> element cascade

  val elements_interval : lo -> hi -> amap -> element cascade

  val build : lo -> hi -> element list -> amap
  val fast_build : lo -> hi -> element list -> amap

  val read: lo -> hi -> amap -> value

end
