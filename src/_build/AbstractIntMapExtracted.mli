(* This code has been extracted from Coq and auto-massaged. Do not modify! *)

module Make (V : sig
  type value

  val dummy : value

  val period : value -> int

  val origin : value -> int option

  val same : value -> value -> bool

  val shift : int -> value -> value

  val subv : int -> int -> value -> int -> value -> bool

  val join : value -> value -> value

  val patch : int -> int -> int -> value -> value -> value
end) : sig
  open V

  type tree = IMLeaf | IMNode of int * value * int * tree * int * tree

  type element = E of int * int * value

  type btree = int * tree

  val is_above : int -> int -> int -> int -> bool

  val test_lwo : int -> int -> element list -> bool

  val compress1 : element -> element list -> element list

  val compress : element list -> element list

  val zero : tree

  val extract_leftmost_element : int -> int -> value -> int -> tree -> element

  val extract_leftmost_residual :
    int -> int -> value -> int -> tree -> int -> tree -> int * tree

  val extract_rightmost_element : int -> int -> value -> int -> tree -> element

  val extract_rightmost_residual :
    int -> int -> value -> int -> tree -> int -> tree -> int * tree

  val make_node_left :
    int -> int -> value -> int -> tree -> int -> tree -> btree

  val make_node_right :
    int -> int -> value -> int -> tree -> int -> tree -> btree

  val make_node : int -> int -> value -> int -> tree -> int -> tree -> btree

  val concat : (int, tree, int, tree) OCamlCoq.prod4 -> int * tree

  val concat_singleton_left : int -> int -> value -> int -> tree -> int * tree

  val concat_singleton_right : int -> tree -> int -> int -> value -> int * tree

  val concat_singleton_left_no_stitching :
    int -> int -> value -> int -> tree -> int * tree

  val concat_singleton_right_no_stitching :
    int -> tree -> int -> int -> value -> int * tree

  val merge : (int, tree, int, tree) OCamlCoq.prod4 -> int * tree

  val sub_singleton : int -> tree -> int -> int -> value -> bool

  val sub : (int, tree, int, tree) OCamlCoq.prod4 -> bool

  val keep_below : int -> int -> tree -> int * tree

  val keep_above : int -> int -> tree -> int * tree

  val keep_interval : int -> int -> int -> tree -> int * tree

  type 'a head = Nil | Cons of 'a * (unit -> 'a head)

  type 'a cascade = unit -> 'a head

  val elements_ : int -> tree -> element head -> element head

  val build_ : btree -> element list -> btree

  val elements_below_ : int -> int -> tree -> element head -> element head

  val elements_above_ : int -> int -> tree -> element head -> element head

  val elements_interval_ :
    int -> int -> int -> tree -> element head -> element head

  val zlength_plus : int -> 'a1 list -> int

  type 'a segment = int * 'a list

  val highest_bit_mask_64 : int -> int

  val highest_bit_mask : int -> int

  val edummy : element

  val custom_split_seg_loop_1 :
       int
    -> int
    -> element list
    -> (int, element, int * element list) OCamlCoq.prod3

  val custom_split_seg_loop_2 :
       int
    -> int
    -> int
    -> element list
    -> (int, element, int * element list) OCamlCoq.prod3

  val custom_split_seg :
       int
    -> int
    -> element segment
    -> (element segment, element, element segment) OCamlCoq.prod3

  val fast_build_rec : element segment -> int -> int -> btree

  val fast_build : int -> int -> element list -> btree

  val summarize : int -> int -> int -> tree -> value

  val read : int -> int -> int * tree -> value

  type amap = (int, int, int, tree) OCamlCoq.prod4

  val public_lo : amap -> int

  val public_hi : amap -> int

  val public_zero : int -> amap

  val public_singleton : int -> int -> value -> amap

  val public_concat : amap -> amap -> amap

  val public_concat_singleton_left : int -> int -> value -> amap -> amap

  val public_concat_singleton_right : amap -> int -> int -> value -> amap

  val public_merge : amap -> amap -> amap

  val public_sub : amap -> amap -> bool

  val public_keep_below : int -> amap -> amap

  val public_keep_above : int -> amap -> amap

  val public_keep_interval : int -> int -> amap -> amap

  val public_elements : amap -> element cascade

  val public_elements_below : int -> amap -> element cascade

  val public_elements_above : int -> amap -> element cascade

  val public_elements_interval : int -> int -> amap -> element cascade

  val public_build : int -> int -> element list -> amap

  val public_fast_build : int -> int -> element list -> amap

  val public_read : int -> int -> amap -> value
end
