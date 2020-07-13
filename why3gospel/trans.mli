(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** The type for WhyML declarations after translation from GOSPEL signatures. A
    GOSPEL signature can be translater to a WhyML declaration [d] ([Gdecl d]) or
    a WhyML module [m] located at [l] and containing multiple declarations [d]
    ([Gmodule(l, m, d)]). *)
type gdecl =
  | Gdecl of Why3.Ptree.decl
  | Gmodule of Why3.Loc.position * Why3.Ptree.ident * gdecl list

val signature : Info.info -> Gospel.Tast.signature -> gdecl list list
(** [signature info s] is the translated [gdecl] corresponding to the GOPEL
   signature [s]. *)
