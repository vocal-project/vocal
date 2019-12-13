(** Temporary "driver" from GOSPEL symbols into WhyML.

    This is a very simple workaround, in order to support translation of
    existing examples of the VOCaL library. In the future, this should resemble
    the drivers technology of Why3, i.e., one should be able to provide a
    driver file mapping GOSPEL symbols into a corresponding counterpart from the
    Why3 standard library. The translation plugin should then consume such
    file, similarly to how Why3 extraction mechanism deals with drivers.
    Finally, this would avoid the dependecy on the [gospel.mlw] file. *)

module I  = Gospel.Identifier
module Ty = Gospel.Ttypes

type syntax_map = string I.Mid.t

(* let driver = Ty.[
 *   ts_integer.ts_ident, "int";
 *   ts_ *)
