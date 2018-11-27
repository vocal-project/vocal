(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2018 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

let swap (a: ('a array)) (i: int) (j: int) : unit =
  let v = a.(i) in begin let o = a.(j) in a.(i) <- o; a.(j) <- v end

