
module M = struct

type 'a t = 'a Vector.t

(* FIXME: can be avoid passing capacity here? *)
let create ~dummy = Vector.create ~capacity:0 ~dummy

let length = Vector.length
let is_empty = Vector.is_empty
let clear = Vector.clear
let top = Vector.top

let iter = Vector.iter
let map = Vector.map
let copy = Vector.copy
let fold = Vector.fold_left

(* FIXME: improve push and pop with Vector.incr/decr_size *)
let pop = Vector.pop
let push = Vector.push

end

include M
include StackExtended.Make(M)




