
(** A list-based implementation of mutable stacks,
    where push, pop and length are O(1). *)

module M = struct

type 'a t = {
   mutable items : 'a list;
   mutable size : int }

let create () =
  { items = []; size = 0 }

let clear s =
  s.items <- [];
  s.size <- 0

let is_empty s =
  s.size = 0

let length s =
  s.size

let push s x =
  s.items <- x :: s.items;
  s.size <- s.size + 1

let pop s =
  match s.items with
  | hd :: tl ->
      s.items <- tl;
      s.size <- s.size - 1;
      hd
  | [] -> assert false

let top s =
  match s.items with
  | hd :: _ -> hd
  | [] -> assert false

let iter s f =
  List.iter f s.items

let map s f =
  { items = List.map f s.items; size = s.size }

let copy s =
  map s (fun x -> x)

let fold s f acc =
  List.fold_left f acc s.items

end

include M
include StackExtended.Make(M)
