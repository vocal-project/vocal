
exception EmptyStructure
exception BrokenInvariant
exception OutOfBound

module BinaryRandomAccessList =
struct

  type 'a tree = Leaf of 'a | Node of int * 'a tree * 'a tree
  type 'a digit = Zero | One of 'a tree
  type 'a rlist = 'a digit list

  let empty : 'a rlist = []

  let is_empty = function
    | [] -> true
    | _ -> false

  let size = function
    | Leaf x -> 1
    | Node (w, _, _) -> w

  let link t1 t2 =
    Node (size t1 + size t2, t1, t2)

  let rec cons_tree t = function
    | [] -> [One t]
    | Zero :: ts -> One t :: ts
    | One t' :: ts -> Zero :: cons_tree (link t t') ts

  let rec uncons_tree = function
    | [] -> raise EmptyStructure
    | [One t] -> (t, [])
    | One t :: ts -> (t, Zero :: ts)
    | Zero :: ts ->
      match uncons_tree ts with
      | Node (_, t1, t2), ts' -> (t1, One t2 :: ts')
      | _ -> raise BrokenInvariant

  let cons x ts =
    cons_tree (Leaf x) ts

  let head ts =
    match uncons_tree ts with
    | (Leaf x, _) -> x
    | _ -> raise BrokenInvariant

  let tail ts =
    let (_,ts') = uncons_tree ts in ts'

  let rec lookup_tree i = function
    | Leaf x -> if i = 0 then x else raise OutOfBound
    | Node (w, t1, t2) ->
      if i < w/2
      then lookup_tree i t1
      else lookup_tree (i - w/2) t2

  let rec update_tree i y = function
    | Leaf x -> if i = 0 then Leaf y else raise OutOfBound
    | Node (w, t1, t2) ->
      if i < w/2
      then Node (w, update_tree i y t1, t2)
      else Node (w, t1, update_tree (i - w/2) y t2)

  let rec lookup i = function
    | [] -> raise OutOfBound
    | Zero :: ts -> lookup i ts
    | One t :: ts ->
      if i < size t
      then lookup_tree i t
      else lookup (i - size t) ts

  let rec update i y = function
    | [] -> raise OutOfBound
    | Zero :: ts -> Zero :: update i y ts
    | One t :: ts ->
      if i < size t
      then One (update_tree i y t) :: ts
      else One t :: update (i - size t) y ts

  end
