(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2018 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type 'a content =
  | Link of ('a content) ref
  | Root of int * 'a

type 'a elem = ('a content) ref



let make (v: 'a) : ('a content) ref = let x = ref (Root (0, v)) in x

let rec find : 'a . (('a content) ref) -> (('a content) ref) =
  fun x ->
    begin match !x with
    | Root (_, _) -> x
    | Link y -> let rx = find y in begin x := (Link rx); rx end
    end

let eq (x: ('a content) ref) (y: ('a content) ref) : bool =
  let a = find x in let b = find y in a == b

let get (x: ('a content) ref) : 'a =
  let xv = find x in
  begin match !xv with
  | Root (_, v) -> v
  | Link _ -> assert false (* absurd *)
  end

let set (x: ('a content) ref) (v: 'a) : unit =
  let rx = find x in
  begin match !rx with
  | Root (r, _) -> rx := (Root (r, v))
  | Link _ -> assert false (* absurd *)
  end

let link (x: ('a content) ref) (y: ('a content) ref) : unit =
  if x == y then begin () end
  else
  begin
    begin match (!x, !y) with
    | (Root (rx, vx), Root (ry, _)) ->
      if rx < ry then begin x := (Link y) end
      else
      begin
        begin
          y := (Link x); if rx = ry then begin x := (Root ((rx + 1), vx)) end
        end end
    | (_, _) -> assert false (* absurd *)
    end end

let union_aux (x: ('a content) ref) (y: ('a content) ref) : unit =
  let a = find x in let b = find y in link a b

let union (x: ('a content) ref) (y: ('a content) ref) : unit =
  ignore (union_aux x y)

