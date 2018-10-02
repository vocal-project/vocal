module Make (V : sig

  type value

  val dummy : value

  val period : value -> int

  val origin : value -> int option

  val same : value -> value -> bool

  val shift : int -> value -> value

  val subv : int -> int -> value -> int -> value -> bool

  val join : value -> value -> value

  val patch : int -> int -> int -> value -> value -> value

end) = struct

  let fitting x y =
    x + 1 = y

  module X =
    AbstractIntMapExtracted.Make(V)

  type value = V.value

  type lo     = int
  type hi     = int
  type base   = int
  type max    = int

  type amap = X.amap

  let lo = X.public_lo

  let hi = X.public_hi

  let zero = X.public_zero

  let singleton base max pat =
    assert (0 <= max);
    X.public_singleton base max pat

  let concat a1 a2 =
    assert (fitting (hi a1) (lo a2));
    X.public_concat a1 a2

  let concat_singleton_left base1 max1 pat1 a2 =
    assert (0 <= max1);
    assert (fitting (base1 + max1) (lo a2));
    X.public_concat_singleton_left base1 max1 pat1 a2

  let concat_singleton_right a1 base2 max2 pat2 =
    assert (fitting (hi a1) base2);
    assert (0 <= max2);
    X.public_concat_singleton_right a1 base2 max2 pat2

  let merge a1 a2 =
    assert (lo a1 = lo a2);
    assert (hi a1 = hi a2);
    X.public_merge a1 a2

  let sub a1 a2 =
    assert (lo a1 = lo a2);
    assert (hi a1 = hi a2);
    X.public_sub a1 a2

  let keep_below =
    X.public_keep_below

  let keep_above =
    X.public_keep_above

  let keep_interval lolimit hilimit a =
    assert (lolimit <= hilimit);
    X.public_keep_interval lolimit hilimit a

  type element = X.element =
    | E of base * max * value

  type 'a cascade =
    unit -> 'a head

   and 'a head = 'a X.head =
    | Nil
    | Cons of 'a * 'a cascade

  let elements =
    X.public_elements

  let elements_below =
    X.public_elements_below

  let elements_above =
    X.public_elements_above

  let elements_interval lolimit hilimit a =
    assert (lolimit <= hilimit);
    X.public_elements_interval lolimit hilimit a

  let build lo hi xs =
    assert (X.test_lwo lo hi xs);
    X.public_build lo hi xs

  let fast_build lo hi xs =
    assert (X.test_lwo lo hi xs);
    X.public_fast_build lo hi xs

  (* TEMPORARY should we publish [summarize] and [squash]? for now, just [read]. *)
  let read lolimit hilimit a =
    assert (lo a <= lolimit);
    assert (lolimit <= hilimit - 1);
    assert (hilimit - 1 <= hi a);
    X.public_read lolimit hilimit a

end
