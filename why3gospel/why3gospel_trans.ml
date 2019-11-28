
module T = Gospel.Tast
open Why3
open Ptree
module Ot = Gospel.Oparsetree

type gdecl =
  | Gdecl of decl
  | Gmodule of ident * gdecl list

let location { Location.loc_start = b; Location.loc_end = e } =
  Loc.extract (b, e)

let dummy_loc = Loc.dummy_position

let get_opt_default f d = function None -> d | Some x -> f x

let mk_id id_str id_loc =
  { id_str; id_ats = []; id_loc }

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

module Term = struct
  module Tt = Gospel.Tterm
  module I  = Gospel.Identifier

  let mk_term term_desc term_loc =
    { term_desc; term_loc }

  let mk_pattern pat_desc pat_loc =
    { pat_desc; pat_loc }

  let ident_of_vsymbol Tt.{vs_name = name} =
    mk_id name.I.id_str (location name.I.id_loc)

  let quant = function
    | Tt.Tforall -> Dterm.DTforall
    | Tt.Texists -> Dterm.DTexists
    | Tt.Tlambda -> Dterm.DTlambda

  let (* rec *) pattern pat =
    let loc = get_opt_default location dummy_loc pat.Tt.p_loc in
    let mk_pattern pat_desc = mk_pattern pat_desc loc in
    let p_node = function
      | _ -> assert false (* TODO *) in
    mk_pattern (p_node pat.Tt.p_node)

  let rec term t =
    let loc = get_opt_default location dummy_loc t.Tt.t_loc in
    let mk_term term_desc = mk_term term_desc loc in
    let t_node = function
      | Tt.Ttrue    -> Ttrue
      | Tt.Tfalse   -> Tfalse
      | Tt.Tconst c -> assert false (* TODO *)
      | Tt.Tvar vs  -> Tident (Qident (ident_of_vsymbol vs))
      | Tt.Tnot t   -> Tnot (term t)
      | Tt.Told t   -> Tat (term t, mk_id Dexpr.old_label loc)
      | Tt.Tif (t1, t2, t3) ->
          Tif (term t1, term t2, term t3)
      | Tt.Tlet (vs, t1, t2) ->
          Tlet (ident_of_vsymbol vs, term t1, term t2)
      | Tt.Tcase (t, pat_term_list) ->
          let f_pair (pat, t) = pattern pat, term t in
          Tcase (term t, List.map f_pair pat_term_list)
      | Tt.Tquant (q, vs_list, trigger, t) -> assert false (* TODO *)
          (* Tquant (quant q,  *)
      | _ -> assert false (* TODO *) in
    mk_term (t_node t.Tt.t_node)
end

let rec longident loc = function
  | Longident.Lident s    -> Qident (mk_id s loc)
  | Longident.Ldot (t, s) -> Qdot (longident loc t, mk_id s loc)
  | _ -> assert false (* TODO? *)

let mk_expr expr_desc loc =
  { expr_desc; expr_loc = location loc }

let ident_of_lb_arg lb = Term.ident_of_vsymbol (T.vs_of_lb_arg lb)

(** given the result type [sp_ret] of a function, a GOSPEL postcondition [post]
    (in the form of [term]), convert it into a Why3's [Ptree] postcondition of
    the form [Loc.position * (pattern * term)] *)
let sp_post sp_ret post =
  let pvar_of_lb_arg_list lb_arg_list =
    let mk_pvar lb = (* create a [Pvar] pattern out of a [Tt.lb_arg] *)
      Term.mk_pattern (Pvar (ident_of_lb_arg lb)) dummy_loc in
    List.map mk_pvar lb_arg_list in
  let pat = Term.mk_pattern (Ptuple (pvar_of_lb_arg_list sp_ret)) dummy_loc in
  dummy_loc, [pat, Term.term post]

let spec val_spec = Term.{
  (* TODO: for now, we ignore the [checks] preconditions *)
  sp_pre     = List.map (fun (t, _) -> term t) val_spec.T.sp_pre;
  sp_post    = List.map (fun p -> sp_post val_spec.sp_ret p) val_spec.sp_post;
  sp_xpost   = assert false (* TODO *);
  sp_reads   = [];
  sp_writes  = List.map term val_spec.sp_wr;
  sp_alias   = [];
  sp_variant = [];
  sp_checkrw = false;
  sp_diverge = false;
  sp_partial = false;
}

let empty_spec = {
  sp_pre     = [];
  sp_post    = [];
  sp_xpost   = [];
  sp_reads   = [];
  sp_writes  = [];
  sp_alias   = [];
  sp_variant = [];
  sp_checkrw = false;
  sp_diverge = false;
  sp_partial = false;
}

let rec core_type Ot.{ ptyp_desc; ptyp_loc } =
  match ptyp_desc with
  | Ptyp_var x ->
      PTtyvar (mk_id x (location ptyp_loc))
  | Ptyp_arrow (_, ty1, ty2) ->
      PTarrow (core_type ty1, core_type ty2)
  | Ptyp_tuple ctl ->
      PTtuple (List.map core_type ctl)
  | Ptyp_constr ({txt; loc}, ctl) ->
      PTtyapp (longident (location loc) txt, List.map core_type ctl)
  | _ -> assert false (* TODO *)

(*
let val_decl vd g =
  let rec flat_ptarrow pty = match pty with
    | PTtyvar _ | PTtuple _ | PTtyapp _ -> [pty]
    | PTarrow (t1, t2) -> flat_ptarrow t1 @ flat_ptarrow t2
    | _ -> assert false (* TODO *) in
  let mk_pre_param lb_arg =
    let loc = Term.(location (T.vs_of_lb_arg lb_arg).vs_name.I.id_loc) in
    let id_ghost = match lb_arg with
      | Lnone _ | Lquestion _ | Lnamed _ ->
          Some (ident_of_lb_arg lb_arg), false
      | Lghost _ -> Some (ident_of_lb_arg lb_arg), true in
    let id, ghost = id_ghost in
    loc, id, ghost in
  (* TODO: when there is no spec, we shall create a Why3's [val] with nameless
     arguments, i.e., we give only the types of the arguments. *)
  let pre_param_list = assert false in
  let spec = match vd.vd_spec with
    | None   -> empty_spec
    | Some s -> spec s in
  let e_any =
  Dlet (mk_ident vd.vd_name, g, Expr.RKnone, assert false (*TODO*)) in
  assert false (*TODO*)
 *)

let signature_item i = match i.T.sig_desc with
  (* GOSPEL-modified decls *)
  | T.Sig_val (vd, g) (* of val_description * ghost *) ->
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
