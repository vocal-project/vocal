
(*@ Specification file, implicitly comment out all lines *)


type ('a,'b) map

open math with implicit instances


(*---------------------------------------------------------------------*)
(** Constructors *)

value empty : ('a,'b) map

function single_bind (k:'a) (v:'b) : ('a,'b) map

notation "`{ k := v }" := (single_bind k v)


function read (m:('a,'b) map) (k:'a) : 'b

function union (m1 m2:('a,'b) map) : ('a,'b) map
  (* asymetric union, with bindings in m1 having priority *)

function update (m:('a,'b) map) (k:'a) (v:'b) : ('a,'b) map
  (* note: upate can be defined as union with a singleton *)

function remove (m:('a,'b) map) (s:'a set) : ('a,'b) map

function dom (m:('a,'b) map) : 'a set

  (* horrible notation compared with (s:map A B) *)


(*---------------------------------------------------------------------*)
(** Predicates *)

(* predicate binds (k:'a) (v:'b) (s:('a,'b) map) : prop *)

predicate incl (s1 s2:('a,'b) map) : prop

predicate foreach (p:'a->'b->prop) (s:('a,'b) map) : prop

predicate fold (f:'a->'b->'c) (m:'c monoid) (s:('a,'b) map) : prop
  (* alias map_reduce *)


(*---------------------------------------------------------------------*)
(** Characterization *)

Section Facts.
Implicit Quantifiers ('a 'b : type) (m* : ('a,'b) map) (k* : 'a) (v* : 'b).

fact dom_empty :
  dom ∅ = false

fact dom_single :
  dom `{ k := v } = `{k}

fact dom_update :
  dom (m[k:= v]) = dom m ∪ `{k}

fact dom_union :
  dom (m1 ∪ m2) = (dom m1 ∪ dom m2)

fact dom_remove :
  dom (m ∖ s) = (dom m) ∖ s

fact read_single :
   `{ k := v }[k] = v

fact read_union :
   (m1 ∪ m2)[k] = (if k ∈ dom m1 then m1[k] else m2[k])

fact read_update :
  m[k1 := v][k2] = (if k1 = k2 then v else m[k2])
  (* can be derived is update is defined using union *)

fact read_remove :
   (m ∖ s)[k] = (if k ∉ s then m[k] else arbitrary)

fact read_remove' :
  k ∉ (dom m ∖ s) ->
  (m ∖ s)[k] = m[k]

fact eq_mem :
  (m1 = m2) = (dom m1 = dom m2 /\ forall k, m1[k] = m2[k])

(* example derived fact:
  M[k:=v1][k:=v2] = M[k:=v2]
*)


End Facts.

