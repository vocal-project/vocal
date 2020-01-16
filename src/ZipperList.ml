(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2018 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type 'a t = {
  left: 'a list;
  right: 'a list;
  len: int;
  }

let empty (_: unit) : 'a t = { left = []; right = []; len = 0 }

let is_empty (z: 'a t) : bool =
  begin match (z.left, z.right) with
  | ([], []) -> true
  | _ -> false
  end

let to_list (z: 'a t) : 'a list = List.rev_append (z.left) (z.right)

let rec peano_length : 'a . ('a list) -> (int) =
  fun l -> begin match l with
    | [] -> 0
    | _ :: s -> (peano_length s) + 1
    end

let make (l: 'a list) : 'a t =
  { left = []; right = l; len = (peano_length l) }

let length (z: 'a t) : int = z.len

let move_right (z: 'a t) : 'a t =
  begin match z.right with
  | [] -> assert false (* absurd *)
  | x :: r -> { left = (x :: (z.left)); right = r; len = (z.len) }
  end

let insert_right (x: 'a) (z: 'a t) : 'a t =
  { left = (z.left); right = (x :: (z.right)); len = ((z.len) + 1) }

let remove_right (z: 'a t) : 'a t =
  begin match z.right with
  | [] -> assert false (* absurd *)
  | _ :: r -> { left = (z.left); right = r; len = ((z.len) - 1) }
  end

let move_all_right (z: 'a t) : 'a t =
  let l = List.rev_append (z.right) (z.left) in
  { left = l; right = []; len = (z.len) }

let move_left (z: 'a t) : 'a t =
  begin match z.left with
  | [] -> assert false (* absurd *)
  | x :: l -> { left = l; right = (x :: (z.right)); len = (z.len) }
  end

let insert_left (x: 'a) (z: 'a t) : 'a t =
  { left = (x :: (z.left)); right = (z.right); len = ((z.len) + 1) }

let remove_left (z: 'a t) : 'a t =
  begin match z.left with
  | [] -> assert false (* absurd *)
  | _ :: r -> { left = r; right = (z.right); len = ((z.len) - 1) }
  end

let move_all_left (z: 'a t) : 'a t =
  let l = List.rev_append (z.left) (z.right) in make l

let is_focused (z: 'a t) : bool =
  begin match z.right with
  | [] -> false
  | _ :: _ -> true
  end

let focused (z: 'a t) : 'a option =
  begin match z.right with
  | [] -> None
  | x :: _ -> Some x
  end
