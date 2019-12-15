(*@ predicate k_values (k: int) (a: int seq) =
      forall i: integer. 0 <= i < length a -> 0 <= a[i] < k *)

(*@ function numeq (a: int seq) (v i j: integer) : integer *)

(*@ axiom numeq_base:
      forall a v lo hi. hi <= lo -> numeq a v lo hi = 0 *)
(*@ axiom num_ind:
      forall a v lo hi. 0 <= lo < hi <= length a ->
      numeq a v lo hi = (if a[lo] = v then 1 else 0) + numeq a v (lo+1) hi *)

(*@ predicate permut (k: integer) (a b: int seq) =
      length a = length b /\
      forall v. 0 <= v < k -> numeq a v 0 (length a) = numeq b v 0 (length b) *)

val counting_sort: int -> int array -> int array -> unit
(*@ counting_sort k a b
      requires 0 < k
      requires k_values k a
      requires length a = length b
      modifies b
      ensures  Seq.sorted b
      ensures  permut k a b *)

val in_place_counting_sort: int -> int array -> unit
(*@ in_place_counting_sort k a
      requires 0 < k
      requires k_values k a
      modifies a
      ensures  Seq.sorted a
      ensures  permut k (old a) a *)
