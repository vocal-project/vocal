
module Make(X: ComparableType.S) = struct
  module M = struct

  type elt = X.t

  let le x y = X.compare x y <= 0

  type t = Empty | Node of t * elt * t

  (* FIXME: provide length in O(1) *)

  let le_root e = function
    | Empty          -> true
    | Node (_, x, _) -> le e x

  let empty =
    Empty

  let is_empty = function
    | Empty -> true
    | Node _ -> false

  let top = function
    | Empty          -> invalid_arg "top"
    | Node (_, x, _) -> x

  let rec add q x = match q with
    | Empty ->
        Node (Empty, x, Empty)
    | Node (l, y, r) ->
        if le x y then
          Node (add r y, x, l)
        else
          Node (add r x, y, l)

  let rec pop = function
    | Empty ->
        assert false
    | Node (Empty, y, r) ->
        assert (r = Empty);
        y, Empty
    | Node (l, y, r) ->
        let x, l = pop l in
        x, Node (r, y, l)

  let rec replace_min x = function
    | Node (l, _, r) ->
        if le_root x l && le_root x r then
          Node (l, x, r)
        else
          let lx = top l in
          if le_root lx r then
            (* lx <= x, rx necessarily *)
            Node (replace_min x l, lx, r)
          else
            (* rx <= x, lx necessarily *)
            Node (l, top r, replace_min x r)
    | Empty ->
        assert false

  let rec merge l r = match l, r with
    | _, Empty ->
        l
    | Node (ll, lx, lr), Node (_, ly, _) ->
        if le lx ly then
          Node (r, lx, merge ll lr)
        else
          let x, l = pop l in
          Node (replace_min x r, ly, l)
    | Empty, _ ->
        assert false

  let remove_top = function
    | Empty          -> invalid_arg "remove_top"
    | Node (l, _, r) -> merge l r

  (** The size of a Braun tree can be computed in time O(log^2(n))

      from
        Three Algorithms on Braun Trees (Functional Pearl)
        Chris Okasaki
        J. Functional Programming 7 (6) 661–666, November 1997 *)

  let rec diff m = function
    | Empty ->
        0
    | Node (l, _, r) ->
        if m = 0 then
          1
        else if m mod 2 = 1 then
          (* m = 2k + 1  *)
          diff (m / 2) l
        else
          (* m = 2k + 2 *)
          diff ((m - 1) / 2) r

  let rec length = function
    | Empty          -> 0
    | Node (l, _, r) -> let m = length r in 1 + 2 * m + diff m l

  let rec iter q f = match q with
    | Empty -> ()
    | Node (l, x, r) -> iter l f; f x; iter r f

  let rec fold q f acc = match q with
    | Empty -> acc
    | Node (l, x, r) ->
      let acc = fold l f acc in
      let acc = f acc x in
      fold r f acc

  end
  module ML = PPriorityQueueExtended.AddLength(M)
  include ML
  include PPriorityQueueExtended.AddOptExn(ML)

end
