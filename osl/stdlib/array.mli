
(*@ use Seq *)

(*@ function seq_of_array: 'a array -> 'a seq *)
(* FIXME @ coercion seq_of_array *)

(*@ function length: 'a array -> integer *)
(* FIXME a definition instead
   function length (a: 'a array) : integer =
   Seq.length (seq_of_array a) *)

(*@ function ([]) : 'a array -> integer -> 'a *)
(* FIXME
   function ([]) (a: 'a array) (i: integer) : 'a
   = Seq.([]) (seq_of_array a) i *)
