(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2018 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

let rec binary_search :
  'a . ('a -> ('a -> (int))) -> (('a array)) -> (int) -> (int) -> 'a -> (int) =
  fun cmp a fromi toi v ->
    if fromi >= toi then begin raise (Not_found) end
    else
    begin
      let mid = fromi + ((toi - fromi) / 2) in
      let c = (cmp (a.(mid))) v in
      if c < 0 then begin binary_search cmp a (mid + 1) toi v end
      else
      begin
        if c > 0 then begin binary_search cmp a fromi mid v end
        else
        begin
          mid end end end

let rec binary_search_left :
  'a . ('a -> ('a -> (int))) -> (('a array)) -> (int) -> (int) -> 'a -> (int) =
  fun cmp a fromi toi v ->
    if fromi >= toi then begin toi end
    else
    begin
      let mid = fromi + ((toi - fromi) / 2) in
      let c = (cmp (a.(mid))) v in
      if c < 0 then begin binary_search_left cmp a (mid + 1) toi v end
      else
      begin
        binary_search_left cmp a fromi mid v end end

let rec unsafe_binary_search_right :
  'a . ('a -> ('a -> (int))) -> (('a array)) -> (int) -> (int) -> 'a -> (int) =
  fun cmp a fromi toi v ->
    if fromi >= toi then begin toi end
    else
    begin
      let mid = fromi + ((toi - fromi) / 2) in
      let c = (cmp (a.(mid))) v in
      if c <= 0 then begin
        unsafe_binary_search_right cmp a (mid + 1) toi v end
      else
      begin
        unsafe_binary_search_right cmp a fromi mid v end end

let binary_search_right (cmp: 'a -> ('a -> (int))) (a: ('a array))
                        (fromi: int) (toi: int) (v: 'a) : int =
  begin
    if not ((0 <= fromi) && ((fromi <= toi) && (toi <= (Array.length a))))
    then begin
      raise (Invalid_argument "") end;
    unsafe_binary_search_right cmp a fromi toi v
  end

let binary_sort (cmp: 'a -> ('a -> (int))) (a: ('a array)) (fromi: int)
                (toi: int) : unit =
  if fromi < (toi - 1)
  then begin
    let o = toi - 1 in
    let o1 = fromi + 1 in
    for k = o1 to o do
      let v = a.(k) in
      let mid = unsafe_binary_search_right cmp a fromi k v in
      begin Array.blit a mid a (mid + 1) (k - mid); a.(mid) <- v end done end

let knuth_shuffle (a: ('a array)) : unit =
  let i = ref 0 in
  while (!i) < (Array.length a) do
    let j = Random.int ((!i) + 1) in
    begin Mach__array__Array63Swap.swap a (!i) j; incr i end done

