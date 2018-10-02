
(* A priority queue implemented with a heap within a vector.

   TODO: add and remove_min_elt can be (slightly) improved
         when we have incr_size and decr_size in Vector *)

module Make(X: ComparableType.S) = struct

  module M = struct

  type elt = X.t

  type t = elt Vector.t

  let create ?(capacity=0) ~dummy =
    Vector.create ~capacity ~dummy

  let length h =
    Vector.length h

  let is_empty h =
    length h = 0

  let clear h =
    Vector.resize h 0

  let add h x =
    let n = length h in
    Vector.resize h (n + 1); (* FIXME: improve using Vector.incr_size *)
    (* moving [x] up in the heap *)
    let rec moveup i =
      let fi = (i - 1) / 2 in
      if i > 0 && X.compare (Vector.get h fi) x > 0 then begin
	Vector.set h i (Vector.get h fi);
	moveup fi
      end else
	Vector.set h i x
    in
    moveup n

  let top h =
    if length h = 0 then invalid_arg "top";
    Vector.get h 0

  let remove_top h =
    let n = length h in
    if n = 0 then invalid_arg "remove_top";
    let n = n - 1 in
    let x = Vector.get h n in
    Vector.resize h n; (* FIXME: improve using Vector.decr_size *)
    (* moving [x] down in the heap *)
    let rec movedown i =
      let j = 2 * i + 1 in
      if j < n then
	let j =
	  let j' = j + 1 in
	  if j' < n && X.compare (Vector.get h j') (Vector.get h j) < 0
          then j' else j in
        let xj = Vector.get h j in
	if X.compare xj x < 0 then begin
	  Vector.set h i xj;
	  movedown j
	end else
	  Vector.set h i x
      else
	Vector.set h i x
    in
    if n > 0 then movedown 0

  let pop h =
    let m = top h in
    remove_top h;
    m

  let iter f h =
    Vector.iter f h

  let fold h f acc =
    Vector.fold_left h f acc

  end

  include M
  include PriorityQueueExtended.Make(M)

end
