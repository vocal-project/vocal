(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Pre-identifiers: these are not unique identifier; they come out of the
    parser and are used in the untyped AST. *)
module Preid : sig
  type t = private {
    pid_str : string;  (** The identifier name. *)
    pid_attrs : string list;  (** The attributes of the identifier. *)
    pid_loc : Location.t;  (** The location of the identifier. *)
  }
  (** The type for pre-identifiers. *)

module Preid : sig
  type t = private {
    pid_str : string;
    pid_attrs : string list;
    pid_loc : Location.t;
  }

  val pp : Format.formatter -> t -> unit

  val create : ?attrs:string list -> ?loc:Location.t -> string -> t

  val add_attr : t -> string -> t
end

  val create : ?attrs:string list -> ?loc:Location.t -> string -> t
  (** [create ~attrs ~loc id] is a new pre-identifier identified with [id] with
      attributes [attrs] and location [loc]. A unique tag is automatically
      affected to the new identifier Default attributes are empty, and default
      location is [Location.none]. *)

module Ident : sig
  type t = private {
    id_str : string;
    id_attrs : string list;
    id_loc : Location.t;
    id_tag : int;
  }

  val pp : Format.formatter -> t -> unit

  val create : ?attrs:string list -> ?loc:Location.t -> string -> t

  val of_preid : Preid.t -> t

  val set_loc : t -> Location.t -> t

  val add_attr : t -> string -> t
end

val neq : Ident.t

val eq : Ident.t
val neq : Ident.t
val none : Ident.t
val some : Ident.t
val nil : Ident.t
val cons : Ident.t

val some : Ident.t

val nil : Ident.t

val cons : Ident.t

(* Utils *)

val prefix : string -> string
val infix : string -> string
val mixfix : string -> string

val is_prefix : string -> bool
val is_infix : string -> bool
val is_mixfix : string -> bool
