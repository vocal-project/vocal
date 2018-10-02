module Make(X: sig type t
  val compare : t -> t -> int end) =
struct
  type elt = X.t
  
  type tree =
    | E
    | T of X.t * tree list
  
  type heap = tree
  
  let empty (_: unit) : heap = E
  
  let is_empty (t: heap) : bool =
    begin match t with
    | E -> true
    | T (_, _) -> false
    end
  
  let find_min (t: heap) : X.t =
    begin match t with
    | E -> assert false (* absurd *)
    | T (x, _) -> x
    end
  
  let merge_heap (h1: tree) (h2: tree) : tree =
    begin match (h1, h2) with
    | (E, h) | (h, E) -> h
    | (T (x1, l1), T (x2, l2)) ->
      if (X.compare x1 x2) <= 0 then begin T (x1, (h2 :: l1)) end
      else
      begin
        T (x2, (h1 :: l2)) end
    end
  
  let merge (t1: heap) (t2: heap) : heap = merge_heap t1 t2
  
  let insert (x: X.t) (t: heap) : heap = let h_x = T (x, []) in merge h_x t
  
  let rec merge_pairs_heap (l: tree list) : tree =
    begin match l with
    | [] -> E
    | h :: [] -> h
    | h1 :: h2 :: r -> merge_heap (merge_heap h1 h2) (merge_pairs_heap r)
    end
  
  let delete_min (t: heap) : heap =
    begin match t with
    | E -> assert false (* absurd *)
    | T (x, l) -> merge_pairs_heap l
    end
end



