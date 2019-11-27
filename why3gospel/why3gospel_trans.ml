
module T = Gospel.Tast
open Why3
open Ptree

type gdecl =
  | Gdecl of decl
  | Gmodule of ident * gdecl list

let location { Location.loc_start = b; Location.loc_end = e } =
  Loc.extract (b, e)

let type_decl td = {
  td_loc    = location td.T.td_loc;
  td_ident  = assert false (*TODO*) (*ident*);
  td_params = assert false (*TODO*) (*ident list*);
  td_vis    = assert false (*TODO*) (*visibility*); (* records only *)
  td_mut    = assert false (*TODO*) (*bool*);       (* records or abstract types *)
  td_inv    = assert false (*TODO*) (*invariant*);  (* records only *)
  td_wit    = assert false (*TODO*) (*(qualid * expr) list*);
  td_def    = assert false (*TODO*) (*type_def*);
  }

let signature_item i = match i.T.sig_desc with
  (* GOSPEL-modified decls *)
  | T.Sig_val _ (* of val_description * ghost *) ->
     assert false (*TODO*)
  | T.Sig_type (_rec_flag, tdl, _gh) ->
     Gdecl (Dtype (List.map type_decl tdl))
  | T.Sig_typext _ (*  of Oparsetree.type_extension *) ->
     assert false (*TODO*)
  | T.Sig_module _ (*  of module_declaration *) ->
     assert false (*TODO*)
  | T.Sig_recmodule _ (* of module_declaration list *) ->
     assert false (*TODO*)
  | T.Sig_modtype _ (*  of module_type_declaration *) ->
     assert false (*TODO*)
  (* OCaml decls *)
  | T.Sig_exception _ (* of type_exception *) ->
     assert false (*TODO*)
  | T.Sig_open _ (* of open_description * ghost *) ->
     assert false (*TODO*)
  | T.Sig_include _ (* of Oparsetree.include_description *) ->
     assert false (*TODO*)
  | T.Sig_class _ (* of Oparsetree.class_description list *) ->
     assert false (*TODO*)
  | T.Sig_class_type _ (* of Oparsetree.class_type_declaration list *) ->
     assert false (*TODO*)
  | T.Sig_attribute _ (* of Oparsetree.attribute *) ->
     assert false (*TODO*)
  | T.Sig_extension _ (* of Oparsetree.extension * Oparsetree.attributes *) ->
     assert false (*TODO*)
  (* GOSPEL-specific decls *)
  | T.Sig_use _ (* of string *) ->
     assert false (*TODO*)
  | T.Sig_function _ (* of function_ *) ->
     assert false (*TODO*)
  | T.Sig_axiom _ (* of axiom *) ->
     assert false (*TODO*)

let signature = List.map signature_item



