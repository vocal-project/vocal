(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Temporary "driver" from GOSPEL symbols into WhyML.

    This is a very simple workaround, in order to support translation of
    existing examples of the VOCaL library. In the future, this should resemble
    the drivers technology of Why3, i.e., one should be able to provide a
    driver file mapping GOSPEL symbols into a corresponding counterpart from the
    Why3 standard library. The translation plugin should then consume such
    file, similarly to how Why3 extraction mechanism deals with drivers. *)

let driver = Hashtbl.create 0

let () =
  List.iter
    (fun (x, y) -> Hashtbl.add driver x y)
    Gospel.Ttypes.
      [
        (ts_integer.ts_ident.id_str, "int");
        ("int", "int63");
        ("mixfix {}", "empty");
        ("mixfix {:_:}", "singleton");
        ("[]", "Nil");
        ("infix ::", "Cons");
      ]

let query_syntax str = Hashtbl.find_opt driver str
