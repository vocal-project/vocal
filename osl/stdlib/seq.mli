
(* FIXME add axioms *)

(*@ type 'a seq *)

(*@ function length: 'a seq -> integer *)

(*@ function ([]): 'a seq -> integer -> 'a *)

(*@ predicate (==) (s1 s2: 'a seq) =
      length s1 = length s2 &&
      forall i. 0 <= i < length s1 -> s1[i] = s2[i] *)

(* FIXME create? *)

(*@ function empty: 'a seq *)

(*@ function ([<-]): 'a seq -> integer -> 'a -> 'a seq *)

(*@ function cons: 'a -> 'a seq -> 'a seq *)
(*@ function snoc: 'a seq -> 'a -> 'a seq *)

(* FIXME singleton? *)

(*@ function ([..]): 'a seq -> integer -> integer -> 'a seq *)
(*@ function ([_..]): 'a seq -> integer -> 'a seq *)
(*@ function ([.._]): 'a seq -> integer -> 'a seq *)

(*@ function (++): 'a seq -> 'a seq -> 'a seq *)
(* + name append? *)

(* hd, tl, rev, mem *)
(* higher-order: map, fold, exists, forall, find, partition *)
(* assoc, mem_assoc? split, combine? *)


