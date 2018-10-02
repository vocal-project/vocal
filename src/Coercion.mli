(*@ use List *)
(*@ use Graph *)

module Make (X: sig
  type key
  type 'a graph = key -> key -> 'a option
  type 'a t
  (*@ model view : 'a graph *)

  (*@ predicate is_empty (g : 'a graph ) :=
        forall x y : key. g x y = None *)

  (*@ predicate add (g1 : 'a graph) (g2 : 'a graph) (x : key) (y : key) (e : 'a) :=
        (forall u v : key. u <> x -> g2 u v = g1 u v) /\
        (forall u v : key. v <> y -> g2 u v = g1 u v) /\
        g2 x y = Some e *)

  val empty : unit -> 'a t
  (*@ r = empty ()
      ensures is_empty r.view *)

  val is_empty : 'a t -> bool
  (*@ r = is_empty m
      ensures r <-> is_empty m.view *)

  val add : 'a t -> key -> key -> 'a -> 'a t
  (*@ res = add m x y e
        ensures add m.view res.view x y e *)

  val get : 'a t -> key -> key -> 'a option
  (*@ r = get g x y
      ensures r = g.view x y *)

  val mem : key -> 'a t -> bool
  (*@ r = mem a g
      ensures r <-> exists y. g.view a y <> None *)

  val elements: 'a t -> key list
  (*@ l = elements m
       ensures forall x y. m.view x y <> None -> mem x l /\ mem y l *)
       
  val eq : key -> key -> bool
  (*@ r = eq x y
      ensures r <-> x = y *)

end) : sig

  type vertex = X.key
  type 'a couple (* #TODO *)
  (*@ model lft : vertex *)
  (*@ model rgt : vertex *)

  type 'a tag = ('a couple) list
  type 'a graph = (vertex,'a) Graph.graph

  (*@ predicate transitive_closure (g1 : 'a graph) (g2 : ('a list) graph) :=
      forall x y. x <> y -> (edge g2 x y <-> exists l : vertex list. path g1 x l y) *)

  (** Expresses that l is a list of composition that create a coercion from x to y.
      We need to be convinced by the correctness of this specification **)
      
  (*@ predicate edge_sequence (g : ('a couple) graph) (l : 'a tag) (x : vertex) (y : vertex) :=
      match l with
        | Nil -> x = y
        | Cons hd tl -> x = hd.lft && g hd.lft hd.rgt = Some hd && edge_sequence g tl hd.rgt y *)

  (*@ predicate consistent (g : 'a graph) :=
      forall x y l l'. path g x l y -> path g x l' y -> l <> l' -> false *)

  type 'a t
  (*@ model view : ('a couple) graph *)
  (*@ model closure : ('a tag) graph *)
  (*@ invariant transitive_closure view closure *) (** transitive closure (+) **)
  (*@ invariant forall x y e. closure x y = Some e -> edge_sequence view e x y *)
  (*@ invariant forall x y. forall l l' : vertex list. l <> l' -> path view x l y -> path view x l' y -> false *) (** No cycle AND no diamond **)
  (*@ invariant forall x. not edge view x x *) (** no edge from x to x **)
  (*@ invariant forall x. not edge closure x x *) (** Same in the closure (non reflexive closure) **)
  
  val empty : unit -> 'a t
  (*@ r = empty ()
      ensures is_empty r.view *)

  val is_empty : 'a t -> bool
  (*@ r = is_empty m
      ensures r <-> is_empty m.view *)
      
  exception Not_found
  val find : 'a t -> vertex -> vertex -> 'a tag
  (*@ l = find g x y
      raises Not_found -> not edge g.closure x y
      ensures edge_sequence g.view l x y *)
    

  exception NotAnEdge
  exception Cycle (** Coercion from x to y, and from y to x **)
  exception AlreadyDefined (** The computation of the transitive closure leads to multiple coercions **)
  exception AlreadyAnEdge (** A Coercion has already been declared between the same types as the new coercion **)

(** This function does not check if the new coercion we want to declare is the same as the previous one **)
  val add : 'a t -> vertex -> vertex -> 'a couple -> 'a t
  (*@ r = add g x y e
      requires x = e.lft /\ y = e.rgt
      raises NotAnEdge -> x = y
      raises Cycle -> cycle (addition g.view x y e)
      raises AlreadyDefined -> let g' = addition g.view x y e in not cycle g' /\ not consistent g'
      raises AlreadyAnEdge -> exists e'. g.view x y = Some e'
      ensures add g.view r.view x y *)
      
(** This function checks if the already existing coercion is the same as the new one, and accept if this is the case
    Because of polymorphism, we need to provide it an equality over the type couple 'a **)
  val add_over : 'a t -> vertex -> vertex -> 'a couple -> ('a couple -> 'a couple -> bool) -> 'a t
  (*@ r = add_over g x y e p
      requires x = e.lft /\ y = e.rgt
      requires forall e e'. p e e' <-> e = e'
      raises NotAnEdge -> x = y
      raises Cycle -> cycle (addition g.view x y e)
      raises AlreadyDefined -> let g' = addition g.view x y e in not cycle g' /\ not consistent g'
      raises AlreadyAnEdge -> exists e'. g.view x y = Some e' /\ e <> e'
      ensures add g.view r.view x y *)
      
(* TODO : We don't want the predicate p to be equivalent to the logical equality. It would be nice to rewrite predicate edge_sequence
on a model for couple 'a, so two coercion with the exact same behaviour but a structural difference will be considered as equal *)

end
