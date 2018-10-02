type 'a head =
| Nil
| Cons of 'a * (unit -> 'a head)

type 'a cascade =
  unit -> 'a head

type 'a producer =
  'a cascade -> 'a cascade

(* -------------------------------------------------------------------------- *)

(* Suspensions. *)

type 'a suspension =
  unit -> 'a

let once_fail =
  fun () ->
    (* A suspension created by [once] has been forced twice. *)
    assert false

let once_force (action : 'a suspension ref) : 'a suspension =
  fun () ->
    let f = !action in
    action := once_fail;
    f()

(* If [f] is a suspension, then [once_suspension f] is a suspension that can
   be called at most once -- if it is called more than once, a runtime error
   will occur. *)

let once_suspension (f : 'a suspension) : 'a suspension =
  once_force (ref f)
  (* We are careful to avoid closing over [f], since that would cause a memory
     leak: we would retain a pointer to [f] even after the suspension has been
     forced. *)

(* -------------------------------------------------------------------------- *)

(* Constructors. *)

let nil =
  fun () -> Nil

let cons x xs =
  fun () -> Cons (x, xs)

let singleton x =
  cons x nil

(* -------------------------------------------------------------------------- *)

(* Destructors. *)

let force xs =
  xs()

let is_empty xs =
  match force xs with
  | Nil ->
      true
  | Cons (_, _) ->
      false

let head xs =
  match force xs with
  | Nil ->
      assert false
  | Cons (x, _) ->
      x

let head_opt xs =
  match force xs with
  | Nil ->
      None
  | Cons (x, _) ->
      Some x

let tail xs =
  match force xs with
  | Nil ->
      assert false
  | Cons (_, xs) ->
      xs

let tail_opt xs =
  match force xs with
  | Nil ->
      None
  | Cons (_, xs) ->
      Some xs

let rec force_drop (n : int) (xs : 'a cascade) : 'a head =
  if n = 0 then
    force xs
  else
    match force xs with
    | Nil ->
        Nil
    | Cons (_, xs) ->
        force_drop (n - 1) xs

let drop (n : int) (xs : 'a cascade) : 'a cascade =
  if n = 0 then
    xs
  else
    fun () ->
      force_drop n xs

let get n xs =
  head (drop n xs)

let get_opt n xs =
  head_opt (drop n xs)

(* -------------------------------------------------------------------------- *)

(* Eager iteration. *)

let rec fold_left f accu xs =
  match force xs with
  | Nil ->
      accu
  | Cons (x, xs) ->
      let accu = f accu x in
      fold_left f accu xs

let rec iter f xs =
  match force xs with
  | Nil ->
      ()
  | Cons (x, xs) ->
      f x;
      iter f xs

let rec length_aux accu xs =
  match force xs with
  | Nil ->
      accu
  | Cons (_, xs) ->
      length_aux (accu + 1) xs

let length xs =
  length_aux 0 xs

let to_list_rev (xs : 'a cascade) : 'a list =
  fold_left (fun ys x -> x :: ys) [] xs

let to_list (xs : 'a cascade) : 'a list =
  List.rev (to_list_rev xs)

let rev (xs : 'a cascade) (k : 'a cascade) =
  fun () -> force (
    fold_left (fun ys x -> cons x ys) k xs
  )

let last1 x xs =
  fold_left (fun _ x -> x) x xs

let last_opt (xs : 'a cascade) : 'a option =
  match force xs with
  | Nil ->
      None
  | Cons (x, xs) ->
      Some (last1 x xs)

let last (xs : 'a cascade) : 'a =
  match force xs with
  | Nil ->
      assert false
  | Cons (x, xs) ->
      last1 x xs

(* TEMPORARY CFML does not support:
let to_channel channel cs =
  fold_left (fun n c ->
    output_char channel c;
    n + 1
  ) 0 cs
 *)

let rec write (set : 'array -> int -> 'a -> unit) (xs : 'a cascade) (a : 'array) i j =
  if i < j then
    match force xs with
    | Nil ->
        i, nil
    | Cons (x, xs) ->
        set a i x;
        write set xs a (i + 1) j
  else
    i, xs

let to_subarray xs a i j =
  write Array.set xs a i j

let to_array xs a =
  to_subarray xs a 0 (Array.length a)

(* TEMPORARY CFML does not support:
let to_subbytes xs b i j =
  write Bytes.set xs b i j

let to_bytes xs b =
  to_subbytes xs b 0 (Bytes.length b)
 *)

let rec any (f : 'a -> bool) (xs : 'a cascade) : bool =
  match force xs with
  | Nil ->
      false
  | Cons (x, xs) ->
      f x || any f xs

let rec all (f : 'a -> bool) (xs : 'a cascade) : bool =
  match force xs with
  | Nil ->
      true
  | Cons (x, xs) ->
      f x && all f xs

let rec find (f : 'a -> bool) (xs : 'a cascade) : 'a option =
  match force xs with
  | Nil ->
      None
  | Cons (x, xs) ->
      if f x then
        Some x
      else
        find f xs

let rec equal eq xs ys =
  match force xs, force ys with
  | Nil, Nil ->
      true
  | Cons (x, xs), Cons (y, ys) ->
      eq x y && equal eq xs ys
  | Nil, Cons (_, _)
  | Cons (_, _), Nil ->
      false

(* -------------------------------------------------------------------------- *)

(* A producer is converted to a cascade by applying it to the empty cascade. *)

let run (p : 'a producer) : 'a cascade =
  p nil

(* -------------------------------------------------------------------------- *)

(* Producers. *)

let identity xs =
  xs

let rec concat xs k =
  fun () ->
    match force xs with
    | Nil ->
        force k
    | Cons (x, xs) ->
        Cons (x, concat xs k)

let rec take (n : int) (xs : 'a cascade) (k : 'a cascade) =
  if n = 0 then
    k
  else
    fun () ->
      match force xs with
      | Nil ->
          force k
      | Cons (x, xs) ->
          Cons (x, take (n - 1) xs k)

let rec unfold (f: 's -> ('a * 's) option) (s : 's) k =
  fun () ->
    match f s with
    | None ->
        force k
    | Some (x, s) ->
        Cons (x, unfold f s k)

let rec replicate n (x : 'a) (k : 'a cascade) =
  if n = 0 then
    k
  else
    fun () ->
      Cons (x, replicate (n - 1) x k)

let rec up i j k =
  fun () ->
    if i < j then begin
      Cons (i, up (i + 1) j k)
    end
    else
      force k

let rec down i j k =
  fun () ->
    if i > j then begin
      Cons (i - 1, down (i - 1) j k)
    end
    else
      force k

let from_option (ox : 'a option) : 'a producer =
  match ox with
  | None ->
      identity
  | Some x ->
      cons x

let rec from_list (xs : 'a list) k =
  fun () ->
    match xs with
    | [] ->
        force k
    | x :: xs ->
        Cons (x, from_list xs k)

let from_iterator (it : unit -> 'a option) k =
  let rec c () =
    match it() with
    | None ->
        force k
    | Some x ->
        Cons (x, c)
  in
  c

(* TEMPORARY CFML does not support:
let rec from_substring (s : string) (i : int) (j : int) k =
  fun () ->
    if i < j then
      Cons (s.[i], from_substring s (i + 1) j k)
    else
      force k

let from_string s =
  from_substring s 0 (String.length s)

let from_file (c : in_channel) : char producer =
  from_iterator (fun () ->
    try
      Some (input_char c)
    with End_of_file ->
      None
  )
 *)

let rec from_subarray (a : 'a array) i j k =
  fun () ->
    if i < j then
      Cons (a.(i), from_subarray a (i + 1) j k)
    else
      force k

let from_array (a : 'a array) : 'a producer =
  from_subarray a 0 (Array.length a)

(* -------------------------------------------------------------------------- *)

(* Infinite cascades. *)

let rec repeat (x : 'a) : 'a cascade =
  fun () ->
    Cons (x, repeat x)
(* TEMPORARY even better, but not supported by CFML?
let repeat x =
  let rec r () = c
  and c = Cons (x, r) in
  r
 *)

let rec ints i : int cascade =
  fun () ->
    Cons (i, ints (i + 1))

let rec iterate1 (f : 'a -> 'a) (x : 'a) : 'a cascade =
  fun () ->
    let y = f x in
    Cons (y, iterate1 f y)

let iterate f x =
  cons x (iterate1 f x)

let cycle xs =
  let rec c () =
    force (xs c)
  in
  c

(* We first define a fixed point constructor for OCaml suspensions; then,
   based on it, we define a fixed point constructor for our suspensions.
   The wrapping and unwrapping around [f] has little cost, since [f] is
   invoked at most once. *)

(* TEMPORARY CFML does not support:
let recursive (f : 'a Lazy.t -> 'a Lazy.t) : 'a Lazy.t =
  (* Thanks to OCaml's [let rec] construct, no need to backpatch a reference. *)
  let rec s = lazy (
    Lazy.force (f s)
  ) in
  s

let from_lazy s =
  fun () -> Lazy.force s

let to_lazy =
  Lazy.from_fun
  (* fun s -> lazy (force s) *)

let recursive (f : 'a suspension -> 'a suspension) : 'a suspension =
  from_lazy (recursive (fun (s : 'a Lazy.t) ->
    to_lazy (f (from_lazy s))
  ))

(* In [recursive], we could wrap the call to [Lazy.force] in a handler for the
   exception [Lazy.Undefined]. This would catch the case where the function [f]
   is not productive, and is not even able to produce the head of the cascade.
   However, this would not catch the cases where the head of the cascade can be
   produced, and a productivity failure occurs later on. The recursive equation
   [xs = 0 :: tail xs] is an example. It does not fail when [xs] is constructed
   or when its head is demanded; it fails when its tail is demanded. In order to
   also catch this mistake, we would have to also wrap the call to [Lazy.force]
   in [from_lazy]. This does not seem to be worth the cost. After all, tracing
   the exception [Lazy.Undefined] is not more difficult than tracing an assertion
   failure. *)
 *)

(* -------------------------------------------------------------------------- *)

(* Transformations. *)

(* The type of [fold] is more general (and low-level) than the type exposed in
   the interface file. The latter contains two occurrences of ['b producer],
   which is synonymous with ['b head suspension -> 'b head suspension],
   whereas internally we allow ['b suspension -> 'b suspension]. *)

let rec fold
  (f : 'a -> 'b suspension -> 'b suspension)
  (xs : 'a cascade) (k : 'b suspension)
: 'b suspension =
  fun () ->
    match force xs with
    | Nil ->
        force k
    | Cons (x, xs) ->
        force (f x (fold f xs k))

(* [map] can be defined in terms of [unfold]. *)

let _map_from_unfold (f : 'a -> 'b) (xs : 'a cascade) : 'b producer =
  unfold (fun xs ->
    match force xs with
    | Nil ->
        None
    | Cons (x, xs) ->
        Some (f x, xs)
  ) xs

(* [map] can be defined in terms of [fold]. *)

let _map_from_fold (f : 'a -> 'b) (xs : 'a cascade) : 'b producer =
  fold (fun x -> cons (f x)) xs

(* A direct definition of [map]. *)

let rec map f xs k =
  fun () ->
    match force xs with
    | Nil ->
        force k
    | Cons (x, xs) ->
        Cons (f x, map f xs k)

let flatten (xss : 'a cascade cascade) : 'a producer =
  fold concat xss

(* TEMPORARY CFML does not support:
let rec memoize xs k =
  from_lazy (to_lazy (fun () ->
    match force xs with
    | Nil ->
        force k
    | Cons (x, xs) ->
        Cons (x, memoize xs k)
  ))
 *)

let rec once xs k =
  once_suspension (fun () ->
    match force xs with
    | Nil ->
        force k
    | Cons (x, xs) ->
        Cons (x, once xs k)
  )

let precompute xs k =
  from_list (to_list xs) k

(* [bind] in the cascade monad is not efficient, as it uses [concat]. *)

let _bind_cascade (xs : 'a cascade) (f : 'a -> 'b cascade) : 'b cascade =
  fold (fun x ys ->
    concat (f x) ys
  ) xs nil

let bind (xs : 'a producer) (f : 'a -> 'b producer) : 'b producer =
  fold f (run xs)

(* The auxiliary [force_filter ...] is equivalent to [force (filter ...)]. *)

let rec filter (f : 'a -> bool) (xs : 'a cascade) (k : 'a cascade) : 'a cascade =
  fun () ->
    force_filter f xs k

and force_filter (f : 'a -> bool) (xs : 'a cascade) (k : 'a cascade) : 'a head =
  match force xs with
  | Nil ->
      force k
  | Cons (x, xs) ->
      if f x then
        Cons (x, filter f xs k)
      else
        force_filter f xs k

let rec uniq1 eq x (ys : 'a cascade) : 'a cascade =
  fun () ->
    force_uniq1 eq x ys

and force_uniq1 eq x ys : 'a head =
  match force ys with
  | Nil ->
      Nil
  | Cons (y, ys) ->
     if eq x y then
       force_uniq1 eq x ys
     else
       Cons (y, uniq1 eq y ys)

let uniq eq (xs : 'a cascade) : 'a cascade =
  fun () ->
    match force xs with
    | Nil ->
        Nil
    | Cons (x, ys) ->
        Cons (x, uniq1 eq x ys)

let rec tail_scanl (f : 's -> 'a -> 's) (s : 's) (xs : 'a cascade) : 's cascade =
  fun () ->
    match force xs with
    | Nil ->
        Nil
    | Cons (x, xs) ->
        let s = f s x in
        Cons (s, tail_scanl f s xs)

let scanl (f : 's -> 'a -> 's) (s : 's) (xs : 'a cascade) : 's cascade =
  cons s (tail_scanl f s xs)

(* -------------------------------------------------------------------------- *)

(* Joining. *)

let rec zipWith (f : 'a -> 'b -> 'c) (xs : 'a cascade) (ys : 'b cascade) k =
  fun () ->
    match force xs with
    | Nil ->
        force k
    | Cons (x, xs) ->
        match force ys with
        | Nil ->
            force k
        | Cons (y, ys) ->
            Cons (f x y, zipWith f xs ys k)

let zip xs ys k =
  zipWith (fun a b -> (a, b)) xs ys k

let rec merge1l cmp x xs ys =
  fun () ->
    match force ys with
    | Nil ->
        Cons (x, xs)
    | Cons (y, ys) ->
        merge1 cmp x xs y ys

and merge1r cmp xs y ys =
  fun () ->
    match force xs with
    | Nil ->
        Cons (y, ys)
    | Cons (x, xs) ->
        merge1 cmp x xs y ys

and merge1 cmp x xs y ys =
  if cmp x y <= 0 then
    Cons (x, merge1r cmp xs y ys)
  else
    Cons (y, merge1l cmp x xs ys)

let merge cmp xs ys =
  fun () ->
    match force xs, force ys with
    | Nil, Nil ->
        Nil
    | Nil, c
    | c, Nil ->
        c
    | Cons (x, xs), Cons (y, ys) ->
        merge1 cmp x xs y ys

let rec interleave xs ys =
  fun () ->
    match force xs with
    | Nil ->
        force ys
    | Cons (x, xs) ->
        Cons (x, interleave ys xs)

(* -------------------------------------------------------------------------- *)

(* Merge sort. For fun. Adapted from Haskell's Prelude. *)

let rec merge_pairs cmp (xss : 'a cascade cascade) : 'a cascade cascade =
  (* This was an instance of [unfold], but has been specialized. *)
  fun () ->
    match force xss with
    | Nil ->
        Nil
    | Cons (xs, xss) ->
        match force xss with
        | Nil ->
            Cons (xs, nil)
        | Cons (ys, xss) ->
            Cons (merge cmp xs ys, merge_pairs cmp xss)

let rec force_mergesort_cons cmp xs (xss : 'a cascade cascade) : 'a head =
  match force xss with
  | Nil ->
      force xs
  | Cons (ys, xss) ->
      force_mergesort_cons cmp (merge cmp xs ys) (merge_pairs cmp xss)

let force_mergesort cmp (xss : 'a cascade cascade) : 'a head =
  match force xss with
  | Nil ->
      Nil
  | Cons (xs, xss) ->
      force_mergesort_cons cmp xs xss

let sort cmp (xs : 'a cascade) : 'a cascade =
  fun () ->
    force_mergesort cmp (map singleton xs nil)

(* -------------------------------------------------------------------------- *)

(* Forking. *)

let unzip xys =
  map Pervasives.fst xys,
  map Pervasives.snd xys

(* TEMPORARY CFML does not support:
let unzip_memoize xys =
  unzip (memoize xys nil)
 *)

(* TEMPORARY does this type exist in the stdlib? *)
type ('a, 'b) sum =
| Left of 'a
| Right of 'b

let is_left = function
  | Left a ->
      cons a
  | Right _ ->
      identity

let is_right = function
  | Left _ ->
      identity
  | Right b ->
      cons b

let dispatch (xs : ('a, 'b) sum cascade) : 'a producer * 'b producer =
  fold is_left xs,
  fold is_right xs
  (* these folds could be optimized like [filter] *)

(* TEMPORARY CFML does not support:
let dispatch_memoize xys =
  dispatch (memoize xys nil)
 *)

let _partition f xs =
  dispatch (map (fun x -> if f x then Left x else Right x) xs nil)

let partition (f : 'a -> bool) (xs : 'a cascade) : 'a producer * 'a producer =
  filter f xs,
  filter (fun x -> not (f x)) xs

(* -------------------------------------------------------------------------- *)

(* Bit packing and unpacking. *)

(* A bit can be represented either as a Boolean or as an integer. *)

let b2i (b : bool) : int =
  if b then 1 else 0

let i2b (i : int) : bool =
  assert (i = 0 || i = 1);
  i = 1

(* [unpack_int k i] is a producer. It produces the [k] least significant bits of
   the integer [i]. It is not lazy; [k] memory cells are allocated immediately.
   The bits are written from left to right: the most significant bit comes
   first. *)

let rec unpack_int (k : int) (i : int) (bs : bool cascade) : bool cascade =
  if k = 0 then
    bs
  else
    (* Among the [k] rightmost bits of [i], compute the most significant one.
       This can be done by shifting [i] to the right by [k - 1] and reading
       the rightmost bit. *)
    let bit = (i lsr (k - 1)) land 1 in
    (* Continue. *)
    cons
      (i2b bit)
      (unpack_int (k - 1) i bs)

(* [unpack k xs] is a consumer of the [k]-bit integer cascade [xs] and a bit
   producer. *)

let unpack (k : int) (xs : int cascade) : bool producer =
  assert (k > 0);
  fold (unpack_int k) xs

(* [pack_int k accu] is a consumer. It reads up to [k] bits from its argument
   and inserts them at the right end of the accumulator, which is shifted
   towards the left. If the end of the cascade is reached before [k] bits have
   been read, an appropriate number of zero bits are inserted at the end as
   padding. The function returns the updated accumulator and the remainder of
   the cascade. *)

let rec pack_int (k : int) (accu : int) (bs : bool cascade) : int * bool cascade =
  if k = 0 then
    accu, bs
  else
    let b, bs =
      match force bs with
      | Nil ->
          0, bs (* padding *)
      | Cons (b, bs) ->
          b2i b, bs
    in
    pack_int (k - 1) (accu lsl 1 lor b) bs

(* [pack k bs] is a consumer of the bit cascade [bs] and a [k]-bit integer
   producer. If the number of elements in the cascade [bs] is not a multiple
   of [k], an appropriate number of zero bits are inserted at the end as
   padding. *)

let pack (k : int) (bs : bool cascade) : int producer =
  assert (k > 0);
  unfold (fun bs ->
    match force bs with
    | Nil ->
        (* If the bit cascade is finished, we are finished too. *)
        None
    | Cons (b, bs) ->
        (* The bit cascade is not finished, so we must produce one integer by
           reading (up to) [k] bits. *)
        let i, bs = pack_int (k - 1) (b2i b) bs in
        Some (i, bs)
  ) bs
