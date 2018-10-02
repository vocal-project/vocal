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
end) =
struct
  open V

  type tree = IMLeaf | IMNode of int * value * int * tree * int * tree

  type element = E of int * int * value

  type btree = int * tree

  (** val is_above : int -> int -> int -> int -> bool **)

  let is_above base1 max1 base2 max2 =
    (let x = 0 in
     base1 <= x && x <= base1 + max1)
    || not
         (let x = 0 in
          base2 <= x && x <= base2 + max2)
       && (base2 - 1) lxor (base2 + max2) < (base1 - 1) lxor (base1 + max1)

  (** val test_lwo : int -> int -> element list -> bool **)

  let rec test_lwo lo hi = function
    | [] -> hi + 1 = lo
    | x :: xs0 ->
        let (E (base, max, _)) = x in
        (lo = base && 0 <= max) && test_lwo (base + max + 1) hi xs0

  (** val compress1 : element -> element list -> element list **)

  let rec compress1 x = function
    | [] -> [x]
    | y :: ys0 ->
        if
          let (E (base1, max1, pat1)) = x in
          let (E (base2, _, pat2)) = y in
          same (shift base1 pat1) (shift base2 pat2)
          && base1 + max1 + 1 = base2
          &&
          match origin pat2 with
          | Some c -> c mod period pat2 = 0
          | None -> true
        then
          compress1
            (let (E (base1, max1, pat1)) = x in
             E
               ( base1
               , ( max1
                 +
                 let (E (_, max, _)) = y in
                 max + 1 )
               , pat1 ))
            ys0
        else x :: compress1 y ys0

  (** val compress : element list -> element list **)

  let compress = function [] -> [] | x :: ys -> compress1 x ys

  (** val zero : tree **)

  let zero = IMLeaf

  (** val extract_leftmost_element :
    int -> int -> value -> int -> tree -> element **)

  let rec extract_leftmost_element base max pat offl = function
    | IMLeaf -> E (base, max, pat)
    | IMNode (maxl, patl, offll, subll, _, _) ->
        extract_leftmost_element (base + offl) maxl patl offll subll

  (** val extract_leftmost_residual :
    int -> int -> value -> int -> tree -> int -> tree -> int * tree **)

  let rec extract_leftmost_residual base max pat offl subl offr subr =
    match subl with
    | IMLeaf -> (base + offr, subr)
    | IMNode (maxl, patl, offll, subll, offlr, sublr) ->
        let basel', subl' =
          extract_leftmost_residual (base + offl) maxl patl offll subll offlr
            sublr
        in
        (base, IMNode (max, pat, basel' - base, subl', offr, subr))

  (** val extract_rightmost_element :
    int -> int -> value -> int -> tree -> element **)

  let rec extract_rightmost_element base max pat offr = function
    | IMLeaf -> E (base, max, pat)
    | IMNode (maxr, patr, _, _, offrr, subrr) ->
        extract_rightmost_element (base + offr) maxr patr offrr subrr

  (** val extract_rightmost_residual :
    int -> int -> value -> int -> tree -> int -> tree -> int * tree **)

  let rec extract_rightmost_residual base max pat offl subl offr = function
    | IMLeaf -> (base + offl, subl)
    | IMNode (maxr, patr, offrl, subrl, offrr, subrr) ->
        let baser', subr' =
          extract_rightmost_residual (base + offr) maxr patr offrl subrl offrr
            subrr
        in
        (base, IMNode (max, pat, offl, subl, baser' - base, subr'))

  (** val make_node_left :
    int -> int -> value -> int -> tree -> int -> tree -> btree **)

  let make_node_left base max pat offl subl offr subr =
    match subl with
    | IMLeaf -> (base, IMNode (max, pat, offl, subl, offr, subr))
    | IMNode (maxl, patl, offll, subll, offlr, sublr) ->
        let (E (base0, max0, pat0)) =
          extract_rightmost_element (base + offl) maxl patl offlr sublr
        in
        if
          same (shift base0 pat0) (shift base pat)
          && base0 + max0 + 1 = base
          &&
          match origin pat with Some c -> c mod period pat = 0 | None -> true
        then
          let basel, subl0 =
            extract_rightmost_residual (base + offl) maxl patl offll subll
              offlr sublr
          in
          ( base0
          , IMNode
              ( max0 + (max + 1)
              , pat0
              , basel - base0
              , subl0
              , offr + (max0 + 1)
              , subr ) )
        else (base, IMNode (max, pat, offl, subl, offr, subr))

  (** val make_node_right :
    int -> int -> value -> int -> tree -> int -> tree -> btree **)

  let make_node_right base max pat offl subl offr subr =
    match subr with
    | IMLeaf -> (base, IMNode (max, pat, offl, subl, offr, subr))
    | IMNode (maxr, patr, offrl, subrl, offrr, subrr) ->
        let (E (base0, max0, pat0)) =
          extract_leftmost_element (base + offr) maxr patr offrl subrl
        in
        if
          same (shift base pat) (shift base0 pat0)
          && base + max + 1 = base0
          &&
          match origin pat0 with
          | Some c -> c mod period pat0 = 0
          | None -> true
        then
          let baser, subr0 =
            extract_leftmost_residual (base + offr) maxr patr offrl subrl offrr
              subrr
          in
          ( base
          , IMNode (max + (max0 + 1), pat, offl, subl, baser - base, subr0) )
        else (base, IMNode (max, pat, offl, subl, offr, subr))

  (** val make_node :
    int -> int -> value -> int -> tree -> int -> tree -> btree **)

  let make_node base max pat offl subl offr subr =
    match subr with
    | IMLeaf -> make_node_left base max pat offl subl offr subr
    | IMNode (maxr, patr, offrl, subrl, offrr, subrr) ->
        let (E (base0, max0, pat0)) =
          extract_leftmost_element (base + offr) maxr patr offrl subrl
        in
        if
          same (shift base pat) (shift base0 pat0)
          && base + max + 1 = base0
          &&
          match origin pat0 with
          | Some c -> c mod period pat0 = 0
          | None -> true
        then
          let baser, subr0 =
            extract_leftmost_residual (base + offr) maxr patr offrl subrl offrr
              subrr
          in
          make_node_left base
            (max + (max0 + 1))
            pat offl subl (baser - base) subr0
        else make_node_left base max pat offl subl offr subr

  (** val concat : (int, tree, int, tree) OCamlCoq.prod4 -> int * tree **)

  let rec concat = function
    | base1, t1, base2, t2 ->
      match t2 with
      | IMLeaf -> (base1, t1)
      | IMNode (max2, pat2, offl2, subl2, offr2, subr2) ->
        match t1 with
        | IMLeaf -> (base2, t2)
        | IMNode (max1, pat1, offl1, subl1, offr1, subr1) ->
            if is_above base1 max1 base2 max2 then
              match subr1 with
              | IMLeaf ->
                  let basel = base1 + offl1 in
                  make_node_right base1 max1 pat1 (basel - base1) subl1
                    (base2 - base1) t2
              | IMNode (_, _, _, _, _, _) ->
                  let base3 = base1 + offl1 in
                  let base4, t3 = concat (base1 + offr1, subr1, base2, t2) in
                  ( base1
                  , IMNode (max1, pat1, base3 - base1, subl1, base4 - base1, t3)
                  )
            else
              match subl2 with
              | IMLeaf ->
                  let baser = base2 + offr2 in
                  make_node_left base2 max2 pat2 (base1 - base2) t1
                    (baser - base2) subr2
              | IMNode (_, _, _, _, _, _) ->
                  let base3, t3 = concat (base1, t1, base2 + offl2, subl2) in
                  let base4 = base2 + offr2 in
                  ( base2
                  , IMNode (max2, pat2, base3 - base2, t3, base4 - base2, subr2)
                  )

  (** val concat_singleton_left :
    int -> int -> value -> int -> tree -> int * tree **)

  let rec concat_singleton_left base1 max1 pat1 base2 t2 =
    match t2 with
    | IMLeaf -> (base1, IMNode (max1, pat1, 0, IMLeaf, max1 + 1, IMLeaf))
    | IMNode (max2, pat2, offl2, subl2, offr2, subr2) ->
        if is_above base1 max1 base2 max2 then
          make_node_right base1 max1 pat1 0 IMLeaf (base2 - base1) t2
        else
          match subl2 with
          | IMLeaf ->
              let subl = IMNode (max1, pat1, 0, IMLeaf, max1 + 1, IMLeaf) in
              let baser = base2 + offr2 in
              make_node_left base2 max2 pat2 (base1 - base2) subl
                (baser - base2) subr2
          | IMNode (_, _, _, _, _, _) ->
              let base3, t1 =
                concat_singleton_left base1 max1 pat1 (base2 + offl2) subl2
              in
              let base4 = base2 + offr2 in
              ( base2
              , IMNode (max2, pat2, base3 - base2, t1, base4 - base2, subr2) )

  (** val concat_singleton_right :
    int -> tree -> int -> int -> value -> int * tree **)

  let rec concat_singleton_right base1 t1 base2 max2 pat2 =
    match t1 with
    | IMLeaf -> (base2, IMNode (max2, pat2, 0, IMLeaf, max2 + 1, IMLeaf))
    | IMNode (max1, pat1, offl1, subl1, offr1, subr1) ->
        if is_above base1 max1 base2 max2 then
          match subr1 with
          | IMLeaf ->
              let basel = base1 + offl1 in
              let subr = IMNode (max2, pat2, 0, IMLeaf, max2 + 1, IMLeaf) in
              make_node_right base1 max1 pat1 (basel - base1) subl1
                (base2 - base1) subr
          | IMNode (_, _, _, _, _, _) ->
              let base3 = base1 + offl1 in
              let base4, t3 =
                concat_singleton_right (base1 + offr1) subr1 base2 max2 pat2
              in
              ( base1
              , IMNode (max1, pat1, base3 - base1, subl1, base4 - base1, t3) )
        else
          make_node_left base2 max2 pat2 (base1 - base2) t1 (max2 + 1) IMLeaf

  (** val concat_singleton_left_no_stitching :
    int -> int -> value -> int -> tree -> int * tree **)

  let rec concat_singleton_left_no_stitching base1 max1 pat1 base2 t2 =
    match t2 with
    | IMLeaf -> (base1, IMNode (max1, pat1, 0, IMLeaf, max1 + 1, IMLeaf))
    | IMNode (max2, pat2, offl2, subl2, offr2, subr2) ->
        if is_above base1 max1 base2 max2 then
          (base1, IMNode (max1, pat1, base1 - base1, zero, base2 - base1, t2))
        else
          let base3, t1 =
            concat_singleton_left_no_stitching base1 max1 pat1 (base2 + offl2)
              subl2
          in
          let base4 = base2 + offr2 in
          (base2, IMNode (max2, pat2, base3 - base2, t1, base4 - base2, subr2))

  (** val concat_singleton_right_no_stitching :
    int -> tree -> int -> int -> value -> int * tree **)

  let rec concat_singleton_right_no_stitching base1 t1 base2 max2 pat2 =
    match t1 with
    | IMLeaf -> (base2, IMNode (max2, pat2, 0, IMLeaf, max2 + 1, IMLeaf))
    | IMNode (max1, pat1, offl1, subl1, offr1, subr1) ->
        if is_above base1 max1 base2 max2 then
          let base3 = base1 + offl1 in
          let base4, t3 =
            concat_singleton_right_no_stitching (base1 + offr1) subr1 base2
              max2 pat2
          in
          (base1, IMNode (max1, pat1, base3 - base1, subl1, base4 - base1, t3))
        else
          let base3 = base2 + max2 + 1 in
          (base2, IMNode (max2, pat2, base1 - base2, t1, base3 - base2, zero))

  (** val merge : (int, tree, int, tree) OCamlCoq.prod4 -> int * tree **)

  let rec merge = function
    | base1, t1, base2, t2 ->
      match t2 with
      | IMLeaf -> (base1, t1)
      | IMNode (max2, pat2, offl2, subl2, offr2, subr2) ->
        match t1 with
        | IMLeaf -> (base1, t1)
        | IMNode (max1, pat1, offl1, subl1, offr1, subr1) ->
            let hi1 = base1 + max1 + 1 in
            let hi2 = base2 + max2 + 1 in
            let lo = Pervasives.max base1 base2 in
            let hi = Pervasives.min hi1 hi2 in
            let basel, subl =
              merge
                ( if base1 = base2 then
                    (base1 + offl1, subl1, base2 + offl2, subl2)
                else if base1 < base2 then
                  let base3, t3 =
                    concat_singleton_right_no_stitching (base1 + offl1) subl1
                      base1
                      (base2 - 1 - base1)
                      pat1
                  in
                  (base3, t3, base2 + offl2, subl2)
                else
                  let base3, t3 =
                    concat_singleton_right_no_stitching (base2 + offl2) subl2
                      base2
                      (base1 - 1 - base2)
                      pat2
                  in
                  (base1 + offl1, subl1, base3, t3) )
            in
            let baser, subr =
              merge
                ( if hi1 = hi2 then (base1 + offr1, subr1, base2 + offr2, subr2)
                else if hi1 < hi2 then
                  let base3, t3 =
                    concat_singleton_left_no_stitching hi1
                      (hi2 - 1 - hi1)
                      (shift (base2 - hi1) pat2)
                      (base2 + offr2) subr2
                  in
                  (base1 + offr1, subr1, base3, t3)
                else
                  let base3, t3 =
                    concat_singleton_left_no_stitching hi2
                      (hi1 - 1 - hi2)
                      (shift (base1 - hi2) pat1)
                      (base1 + offr1) subr1
                  in
                  (base3, t3, base2 + offr2, subr2) )
            in
            make_node lo
              (hi - 1 - lo)
              (join (shift (base1 - lo) pat1) (shift (base2 - lo) pat2))
              (basel - lo) subl (baser - lo) subr

  (** val sub_singleton : int -> tree -> int -> int -> value -> bool **)

  let rec sub_singleton base1 t1 base2 max2 pat2 =
    match t1 with
    | IMLeaf -> true
    | IMNode (max1, pat1, offl1, subl1, offr1, subr1) ->
        let esab1 = base1 + max1 in
        let esab2 = base2 + max2 in
        if esab1 < base2 then
          sub_singleton (base1 + offr1) subr1 base2 max2 pat2
        else if esab2 < base1 then
          sub_singleton (base1 + offl1) subl1 base2 max2 pat2
        else
          let base = Pervasives.max base1 base2 in
          let esab = Pervasives.min esab1 esab2 in
          ( subv (esab - base) (base1 - base) pat1 (base2 - base) pat2
          && ( base1 <= base2
             || ( match origin pat2 with
                | Some o -> (base2 - base1 + o) mod period pat2 = 0
                | None -> true )
                && sub_singleton (base1 + offl1) subl1 base2 max2 pat2 ) )
          && ( esab2 <= esab1
             || ( match origin pat2 with
                | Some o -> (base2 - (esab1 + 1) + o) mod period pat2 = 0
                | None -> true )
                && sub_singleton (base1 + offr1) subr1 base2 max2 pat2 )

  (** val sub : (int, tree, int, tree) OCamlCoq.prod4 -> bool **)

  let rec sub = function
    | base1, t1, base2, t2 ->
      match t1 with
      | IMLeaf -> true
      | IMNode (max1, pat1, offl1, subl1, offr1, subr1) ->
        match t2 with
        | IMLeaf -> true
        | IMNode (max2, pat2, offl2, subl2, offr2, subr2) ->
            let esab1 = base1 + max1 in
            let esab2 = base2 + max2 in
            if esab1 < base2 then
              ( sub (base1, t1, base2 + offl2, subl2)
              && sub_singleton (base1 + offr1) subr1 base2 max2 pat2 )
              && sub (base1 + offr1, subr1, base2 + offr2, subr2)
            else if esab2 < base1 then
              ( sub (base1 + offl1, subl1, base2 + offl2, subl2)
              && sub_singleton (base1 + offl1) subl1 base2 max2 pat2 )
              && sub (base1, t1, base2 + offr2, subr2)
            else
              let base = Pervasives.max base1 base2 in
              let esab = Pervasives.min esab1 esab2 in
              ( subv (esab - base) (base1 - base) pat1 (base2 - base) pat2
              &&
              if base1 = base2 then
                sub (base1 + offl1, subl1, base2 + offl2, subl2)
              else if base1 < base2 then sub (base1, t1, base2 + offl2, subl2)
              else
                ( match origin pat2 with
                | Some o -> (base2 - base1 + o) mod period pat2 = 0
                | None -> true )
                && sub (base1 + offl1, subl1, base2, t2) )
              &&
              if esab1 = esab2 then
                sub (base1 + offr1, subr1, base2 + offr2, subr2)
              else if esab2 < esab1 then sub (base1, t1, base2 + offr2, subr2)
              else
                ( match origin pat2 with
                | Some o -> (base2 - (esab1 + 1) + o) mod period pat2 = 0
                | None -> true )
                && sub (base1 + offr1, subr1, base2, t2)

  (** val keep_below : int -> int -> tree -> int * tree **)

  let rec keep_below limit base t =
    match t with
    | IMLeaf -> (base, t)
    | IMNode (max, pat, offl, subl, offr, subr) ->
        if limit < base then keep_below limit (base + offl) subl
        else if base + max + 1 < limit then
          let base1 = base + offl in
          let base3, t3 = keep_below limit (base + offr) subr in
          (base, IMNode (max, pat, base1 - base, subl, base3 - base, t3))
        else if limit = base then (base + offl, subl)
        else if limit < base + max + 1 then
          concat_singleton_right_no_stitching (base + offl) subl base
            (limit - 1 - base)
            pat
        else
          let base1 = base + offl in
          (base, IMNode (max, pat, base1 - base, subl, limit - base, zero))

  (** val keep_above : int -> int -> tree -> int * tree **)

  let rec keep_above limit base t =
    match t with
    | IMLeaf -> (base, t)
    | IMNode (max, pat, offl, subl, offr, subr) ->
        if base + max + 1 < limit then keep_above limit (base + offr) subr
        else if limit < base then
          let base1, t1 = keep_above limit (base + offl) subl in
          let base3 = base + offr in
          (base, IMNode (max, pat, base1 - base, t1, base3 - base, subr))
        else if limit = base + max + 1 then (base + offr, subr)
        else if base < limit then
          concat_singleton_left_no_stitching limit
            (base + max - limit)
            (shift (base - limit) pat)
            (base + offr) subr
        else
          let base3 = base + offr in
          (base, IMNode (max, pat, limit - base, zero, base3 - base, subr))

  (** val keep_interval : int -> int -> int -> tree -> int * tree **)

  let rec keep_interval lolimit hilimit base t =
    match t with
    | IMLeaf -> (base, t)
    | IMNode (max, pat, offl, subl, offr, subr) ->
        if hilimit <= base then
          keep_interval lolimit hilimit (base + offl) subl
        else if base + max + 1 <= lolimit then
          keep_interval lolimit hilimit (base + offr) subr
        else if lolimit <= base && base + max + 1 <= hilimit then
          let base1, t1 =
            if lolimit = base then (lolimit, zero)
            else keep_above lolimit (base + offl) subl
          in
          let base3, t3 =
            if hilimit = base + max + 1 then (hilimit, zero)
            else keep_below hilimit (base + offr) subr
          in
          (base, IMNode (max, pat, base1 - base, t1, base3 - base, t3))
        else if base + max + 1 = hilimit then
          ( lolimit
          , let max0 = base + max - lolimit in
            IMNode
              (max0, shift (base - lolimit) pat, 0, IMLeaf, max0 + 1, IMLeaf)
          )
        else if base + max + 1 < hilimit then
          let base2, t2 = keep_below hilimit (base + offr) subr in
          concat_singleton_left_no_stitching lolimit
            (base + max - lolimit)
            (shift (base - lolimit) pat)
            base2 t2
        else if lolimit = base then
          ( base
          , let max0 = hilimit - 1 - base in
            IMNode (max0, pat, 0, IMLeaf, max0 + 1, IMLeaf) )
        else if lolimit < base then
          let base1, t1 = keep_above lolimit (base + offl) subl in
          concat_singleton_right_no_stitching base1 t1 base
            (hilimit - 1 - base)
            pat
        else
          ( lolimit
          , if lolimit = hilimit then zero
            else
              let max0 = hilimit - 1 - lolimit in
              IMNode
                (max0, shift (base - lolimit) pat, 0, IMLeaf, max0 + 1, IMLeaf)
          )

  type 'a head = Nil | Cons of 'a * (unit -> 'a head)

  type 'a cascade = unit -> 'a head

  (** val elements_ : int -> tree -> element head -> element head **)

  let rec elements_ base t h =
    match t with
    | IMLeaf -> h
    | IMNode (max, pat, offl, subl, offr, subr) ->
        let baser = base + offr in
        let k _ = elements_ baser subr h in
        let x = E (base, max, pat) in
        let c = Cons (x, k) in
        elements_ (base + offl) subl c

  (** val build_ : btree -> element list -> btree **)

  let rec build_ bt1 = function
    | [] -> bt1
    | e :: xs0 ->
        let (E (base2, max2, pat2)) = e in
        build_
          (let base1, t1 = bt1 in
           concat_singleton_right_no_stitching base1 t1 base2 max2 pat2)
          xs0

  (** val elements_below_ :
    int -> int -> tree -> element head -> element head **)

  let rec elements_below_ limit base t h =
    match t with
    | IMLeaf -> h
    | IMNode (max, pat, offl, subl, offr, subr) ->
        if limit < base then elements_below_ limit (base + offl) subl h
        else if base + max + 1 < limit then
          let baser = base + offr in
          let k _ = elements_below_ limit baser subr h in
          let x = E (base, max, pat) in
          let c = Cons (x, k) in
          elements_ (base + offl) subl c
        else if limit = base then elements_ (base + offl) subl h
        else
          let c = Cons (E (base, limit - 1 - base, pat), fun _ -> h) in
          elements_ (base + offl) subl c

  (** val elements_above_ :
    int -> int -> tree -> element head -> element head **)

  let rec elements_above_ limit base t h =
    match t with
    | IMLeaf -> h
    | IMNode (max, pat, offl, subl, offr, subr) ->
        if base + max + 1 < limit then
          elements_above_ limit (base + offr) subr h
        else if limit < base then
          let baser = base + offr in
          let k _ = elements_ baser subr h in
          let x = E (base, max, pat) in
          let c = Cons (x, k) in
          elements_above_ limit (base + offl) subl c
        else if limit = base + max + 1 then elements_ (base + offr) subr h
        else
          let baser = base + offr in
          let k _ = elements_ baser subr h in
          let x = E (limit, base + max - limit, shift (base - limit) pat) in
          Cons (x, k)

  (** val elements_interval_ :
    int -> int -> int -> tree -> element head -> element head **)

  let rec elements_interval_ lolimit hilimit base t h =
    match t with
    | IMLeaf -> h
    | IMNode (max, pat, offl, subl, offr, subr) ->
        if hilimit <= base then
          elements_interval_ lolimit hilimit (base + offl) subl h
        else if base + max + 1 <= lolimit then
          elements_interval_ lolimit hilimit (base + offr) subr h
        else if lolimit <= base && base + max + 1 <= hilimit then
          let x = E (base, max, pat) in
          let k =
            if hilimit = base + max + 1 then fun _ -> h
            else
              let baser = base + offr in
              fun _ -> elements_below_ hilimit baser subr h
          in
          let c = Cons (x, k) in
          if lolimit = base then c
          else elements_above_ lolimit (base + offl) subl c
        else if base + max + 1 < hilimit then
          let x =
            E (lolimit, base + max - lolimit, shift (base - lolimit) pat)
          in
          let baser = base + offr in
          let k _ = elements_below_ hilimit baser subr h in
          Cons (x, k)
        else if lolimit < base then
          let x = E (base, hilimit - 1 - base, pat) in
          let c = Cons (x, fun _ -> h) in
          elements_above_ lolimit (base + offl) subl c
        else if lolimit = hilimit then h
        else
          let x =
            E (lolimit, hilimit - 1 - lolimit, shift (base - lolimit) pat)
          in
          Cons (x, fun _ -> h)

  (** val zlength_plus : int -> 'a1 list -> int **)

  let rec zlength_plus n = function
    | [] -> n
    | _ :: xs0 -> zlength_plus (n + 1) xs0

  type 'a segment = int * 'a list

  (** val highest_bit_mask_64 : int -> int **)

  let highest_bit_mask_64 x =
    let x0 =
      let x0 =
        let x0 =
          let x0 =
            let x0 =
              let x0 = x lor (x lsr 1) in
              x0 lor (x0 lsr (fun p -> 2 * p) 1)
            in
            x0 lor (x0 lsr (fun p -> 2 * p) ((fun p -> 2 * p) 1))
          in
          x0
          lor (x0 lsr (fun p -> 2 * p) ((fun p -> 2 * p) ((fun p -> 2 * p) 1)))
        in
        x0
        lor x0
            lsr (fun p -> 2 * p)
                  ((fun p -> 2 * p) ((fun p -> 2 * p) ((fun p -> 2 * p) 1)))
      in
      x0
      lor x0
          lsr (fun p -> 2 * p)
                ((fun p -> 2 * p)
                   ((fun p -> 2 * p) ((fun p -> 2 * p) ((fun p -> 2 * p) 1))))
    in
    x0 lxor (x0 lsr 1)

  (** val highest_bit_mask : int -> int **)

  let highest_bit_mask x =
    if (fun _ -> true) x then highest_bit_mask_64 x
    else (fun _ -> assert false) x

  (** val edummy : element **)

  let edummy = E (0, 0, dummy)

  (** val custom_split_seg_loop_1 :
    int -> int -> element list -> (int, element, int * element list)
    OCamlCoq.prod3 **)

  let rec custom_split_seg_loop_1 c n xs =
    if n = 0 then (0, edummy, (0, []))
    else
      match xs with
      | [] -> (0, edummy, (0, []))
      | x :: xs0 ->
          let n0 = n - 1 in
          if
            let (E (base, max, _)) = x in
            base <= 0 && 0 <= base + max
          then (c, x, (n0, xs0))
          else
            let c0 = c + 1 in
            custom_split_seg_loop_1 c0 n0 xs0

  (** val custom_split_seg_loop_2 :
    int -> int -> int -> element list -> (int, element, int * element list)
    OCamlCoq.prod3 **)

  let rec custom_split_seg_loop_2 hbmsig c n xs =
    if n = 0 then (0, edummy, (0, []))
    else
      match xs with
      | [] -> (0, edummy, (0, []))
      | x :: xs0 ->
          let n0 = n - 1 in
          if
            let (E (base, max, _)) = x in
            not ((base - 1) lxor (base + max) land hbmsig = 0)
          then (c, x, (n0, xs0))
          else
            let c0 = c + 1 in
            custom_split_seg_loop_2 hbmsig c0 n0 xs0

  (** val custom_split_seg :
    int -> int -> element segment -> (element segment, element, element
    segment) OCamlCoq.prod3 **)

  let custom_split_seg lo hi xseg =
    if lo <= 0 && 0 <= hi then
      let n, xs = xseg in
      let c, x, zseg = custom_split_seg_loop_1 0 n xs in
      ((c, xs), x, zseg)
    else
      let hbmsig = highest_bit_mask ((lo - 1) lxor hi) in
      let n, xs = xseg in
      let c, x, zseg = custom_split_seg_loop_2 hbmsig 0 n xs in
      ((c, xs), x, zseg)

  (** val fast_build_rec : element segment -> int -> int -> btree **)

  let rec fast_build_rec x lo hi =
    let n, xs = x in
    if n = 0 then (lo, zero)
    else if n = 1 then
      let x0 =
        match xs with [] -> assert false (* absurd case *) | x0 :: _ -> x0
      in
      let (E (base, max, pat)) = x0 in
      (base, IMNode (max, pat, 0, IMLeaf, max + 1, IMLeaf))
    else
      let half1, pivot, half2 = custom_split_seg lo hi x in
      let (E (base, max, pat)) = pivot in
      let call1 = fast_build_rec half1 lo (base - 1) in
      let call2 = fast_build_rec half2 (base + max + 1) hi in
      let base1, t1 = call1 in
      let base3, t3 = call2 in
      (base, IMNode (max, pat, base1 - base, t1, base3 - base, t3))

  (** val fast_build : int -> int -> element list -> btree **)

  let fast_build lo hi xs =
    let cxs = compress xs in
    fast_build_rec (zlength_plus 0 cxs, cxs) lo hi

  (** val summarize : int -> int -> int -> tree -> value **)

  let rec summarize lo hi base = function
    | IMLeaf -> dummy
    | IMNode (max, pat, offl, subl, offr, subr) ->
        if base <= lo then
          let lo2 = base + max + 1 in
          if hi < lo2 then shift base pat
          else
            patch base lo2 hi (shift base pat)
              (summarize (base + max + 1) hi (base + offr) subr)
        else
          patch lo base hi
            (summarize lo (base - 1) (base + offl) subl)
            (let lo2 = base + max + 1 in
             if hi < lo2 then shift base pat
             else
               patch base lo2 hi (shift base pat)
                 (summarize (base + max + 1) hi (base + offr) subr))

  (** val read : int -> int -> (int * tree) -> value **)

  let read lolimit hilimit = function
    | base, t ->
        let base0, t0 = keep_interval lolimit hilimit base t in
        summarize lolimit (hilimit - 1) base0 t0

  type amap = (int, int, int, tree) OCamlCoq.prod4

  (** val public_lo : amap -> int **)

  let public_lo = function lo, _, _, _ -> lo

  (** val public_hi : amap -> int **)

  let public_hi = function _, hi, _, _ -> hi

  (** val public_zero : int -> amap **)

  let public_zero lo = (lo, lo - 1, lo, zero)

  (** val public_singleton : int -> int -> value -> amap **)

  let public_singleton base max pat =
    (base, base + max, base, IMNode (max, pat, 0, IMLeaf, max + 1, IMLeaf))

  (** val public_concat : amap -> amap -> amap **)

  let public_concat a1 a2 =
    let lo1, _, base1, t1 = a1 in
    let _, hi2, base2, t2 = a2 in
    let filtered_var = concat (base1, t1, base2, t2) in
    let base, t = filtered_var in
    (lo1, hi2, base, t)

  (** val public_concat_singleton_left : int -> int -> value -> amap -> amap **)

  let public_concat_singleton_left base1 max1 pat1 = function
    | _, hi2, base2, t2 ->
        let filtered_var = concat_singleton_left base1 max1 pat1 base2 t2 in
        let base, t = filtered_var in
        (base1, hi2, base, t)

  (** val public_concat_singleton_right :
    amap -> int -> int -> value -> amap **)

  let public_concat_singleton_right a1 base2 max2 pat2 =
    let lo1, _, base1, t1 = a1 in
    let filtered_var = concat_singleton_right base1 t1 base2 max2 pat2 in
    let base, t = filtered_var in
    (lo1, base2 + max2, base, t)

  (** val public_merge : amap -> amap -> amap **)

  let public_merge a1 a2 =
    let lo1, hi1, base1, t1 = a1 in
    let _, _, base2, t2 = a2 in
    let filtered_var = merge (base1, t1, base2, t2) in
    let base, t = filtered_var in
    (lo1, hi1, base, t)

  (** val public_sub : amap -> amap -> bool **)

  let public_sub a1 a2 =
    let _, _, base1, t1 = a1 in
    let _, _, base2, t2 = a2 in
    sub (base1, t1, base2, t2)

  (** val public_keep_below : int -> amap -> amap **)

  let public_keep_below limit a =
    match a with lo, hi, base, t ->
      let filtered_var = limit <= lo in
      if filtered_var then (lo, lo - 1, lo, zero)
      else
        let filtered_var0 = hi < limit in
        if filtered_var0 then a
        else
          let filtered_var1 = keep_below limit base t in
          let base0, t0 = filtered_var1 in
          (lo, limit - 1, base0, t0)

  (** val public_keep_above : int -> amap -> amap **)

  let public_keep_above limit a =
    match a with lo, hi, base, t ->
      let filtered_var = hi < limit in
      if filtered_var then
        let lo0 = hi + 1 in
        (lo0, lo0 - 1, lo0, zero)
      else
        let filtered_var0 = limit <= lo in
        if filtered_var0 then a
        else
          let filtered_var1 = keep_above limit base t in
          let base0, t0 = filtered_var1 in
          (limit, hi, base0, t0)

  (** val public_keep_interval : int -> int -> amap -> amap **)

  let public_keep_interval lolimit hilimit a =
    match a with lo, hi, base, t ->
      let filtered_var = hilimit <= lo || hi < lolimit in
      if filtered_var then
        let lo0 = if hilimit <= lo then lo else hi + 1 in
        (lo0, lo0 - 1, lo0, zero)
      else
        let filtered_var0 = (lolimit <= lo, hi < hilimit) in
        let b, b0 = filtered_var0 in
        if b then
          if b0 then a
          else
            let filtered_var1 = keep_below hilimit base t in
            let base0, t0 = filtered_var1 in
            (lo, hilimit - 1, base0, t0)
        else if b0 then
          let filtered_var1 = keep_above lolimit base t in
          let base0, t0 = filtered_var1 in
          (lolimit, hi, base0, t0)
        else
          let filtered_var1 = keep_interval lolimit hilimit base t in
          let base0, t0 = filtered_var1 in
          (lolimit, hilimit - 1, base0, t0)

  (** val public_elements : amap -> element cascade **)

  let public_elements a _ =
    let _, _, base, t = a in
    elements_ base t Nil

  (** val public_elements_below : int -> amap -> element cascade **)

  let public_elements_below limit a _ =
    let _, _, base, t = a in
    elements_below_ limit base t Nil

  (** val public_elements_above : int -> amap -> element cascade **)

  let public_elements_above limit a _ =
    let _, _, base, t = a in
    elements_above_ limit base t Nil

  (** val public_elements_interval : int -> int -> amap -> element cascade **)

  let public_elements_interval lolimit hilimit a _ =
    let _, _, base, t = a in
    elements_interval_ lolimit hilimit base t Nil

  (** val public_build : int -> int -> element list -> amap **)

  let public_build lo hi xs =
    let filtered_var = build_ (lo, zero) (compress xs) in
    let base, t = filtered_var in
    (lo, hi, base, t)

  (** val public_fast_build : int -> int -> element list -> amap **)

  let public_fast_build lo hi xs =
    let filtered_var = fast_build lo hi xs in
    let base, t = filtered_var in
    (lo, hi, base, t)

  (** val public_read : int -> int -> amap -> value **)

  let public_read lolimit hilimit = function
    | _, _, base, t -> read lolimit hilimit (base, t)
end
