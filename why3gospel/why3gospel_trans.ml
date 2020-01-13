(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module T = Gospel.Tast
module Ot = Gospel.Oparsetree
open Why3
open Ptree
open Why3gospel_driver

type gdecl =
  | Gdecl of decl
  | Gmodule of Loc.position * ident * gdecl list

let location { Gospel.Location.loc_start = b; Gospel.Location.loc_end = e } =
  Loc.extract (b, e)

let dummy_loc = Loc.dummy_position

let map_opt_default f d = function None -> d | Some x -> f x

let mk_id id_str id_loc =
  let id_str = match id_str with
    | "mixfix [_]" -> "mixfix []"
    (* FIXME: many other cases; see src/core/ident.ml in Why3 sources *)
    | _ -> id_str in
  { id_str; id_ats = []; id_loc }

let mk_field ~mut:f_mutable ~ghost:f_ghost f_loc f_ident f_pty  =
  { f_loc; f_ident; f_pty; f_mutable; f_ghost }

module Term = struct
  module Tt = Gospel.Tterm
  module Ty = Gospel.Ttypes
  module I  = Gospel.Identifier

  let mk_term term_desc term_loc =
    { term_desc; term_loc }

  let mk_pattern pat_desc pat_loc =
    { pat_desc; pat_loc }

  let ident_of_vsymbol Tt.{vs_name = name} =
    mk_id name.I.id_str (location name.I.id_loc)

  let ident_of_tvsymbol Ty.{tv_name = name} =
    mk_id name.I.id_str (location name.I.id_loc)

  let ident_of_lsymbol Tt.{ls_name = name} =
    mk_id name.I.id_str (location name.I.id_loc)

  let quant = function
    | Tt.Tforall -> Dterm.DTforall
    | Tt.Texists -> Dterm.DTexists
    | Tt.Tlambda -> Dterm.DTlambda

  let rec pattern pat =
    let loc = map_opt_default location dummy_loc pat.Tt.p_loc in
    let mk_pattern pat_desc = mk_pattern pat_desc loc in
    let p_node = function
      | Tt.Pwild         -> Pwild
      | Tt.Pvar vs       -> Pvar (ident_of_vsymbol vs)
      | Tt.Por  (p1, p2) -> Por (pattern p1, pattern p2)
      | Tt.Pas  (p, vs)  -> Pas (pattern p, ident_of_vsymbol vs, false)
      | Tt.Papp (ls, pat_list) when Tt.is_fs_tuple ls ->
          Ptuple (List.map pattern pat_list)
      | Tt.Papp (ls, pat_list) ->
          Papp (Qident (ident_of_lsymbol ls), List.map pattern pat_list) in
    mk_pattern (p_node pat.Tt.p_node)

  let rec ty Ty.{ty_node} = match ty_node with
    | Ty.Tyvar {tv_name} ->
        PTtyvar (mk_id tv_name.id_str (location tv_name.id_loc))
    | Ty.Tyapp (ts, tyl) when Ty.is_ts_tuple ts ->
        PTtuple (List.map ty tyl)
    | Ty.Tyapp (ts, tyl) when Ty.is_ts_arrow ts ->
        let rec arrow_of_pty_list = function
          | [] -> assert false
          | [pty] -> pty
          | arg :: ptyl -> PTarrow (arg, arrow_of_pty_list ptyl) in
        arrow_of_pty_list (List.map ty tyl)
    | Ty.Tyapp ({ts_ident}, tyl) -> let loc = location ts_ident.id_loc in
        let id_str = match query_syntax ts_ident.id_str with
            | None   -> ts_ident.id_str
            | Some s -> s in
        let qualid = mk_id id_str loc in
        PTtyapp (Qident qualid, List.map ty tyl)

  let binder_of_vsymbol vs =
    let loc = vs.Tt.vs_name.I.id_loc in
    let id  = ident_of_vsymbol vs in
    let pty = ty vs.vs_ty in
    location loc, Some id, false, Some pty

  let param_of_vsymbol Tt.{vs_name; vs_ty} =
    let loc = location vs_name.I.id_loc in
    let id  = mk_id vs_name.I.id_str loc in
    let pty = ty vs_ty in
    loc, Some id, false, pty

  let binop = function
    | Tt.Tand      -> Dterm.DTand
    | Tt.Tand_asym -> Dterm.DTand_asym
    | Tt.Tor       -> Dterm.DTor
    | Tt.Tor_asym  -> Dterm.DTor_asym
    | Tt.Timplies  -> Dterm.DTimplies
    | Tt.Tiff      -> Dterm.DTiff

  let rec term t =
    let loc = map_opt_default location dummy_loc t.Tt.t_loc in
    let mk_term term_desc = mk_term term_desc loc in
    let t_node = function
      | Tt.Ttrue    -> Ttrue
      | Tt.Tfalse   -> Tfalse
      | Tt.Tvar vs  -> Tident (Qident (ident_of_vsymbol vs))
      | Tt.Tnot t   -> Tnot (term t)
      | Tt.Told t   -> Tat (term t, mk_id Dexpr.old_label loc)
      | Tt.Tconst c -> begin match c with
          | Pconst_integer (s, None) -> (* FIXME: check that [neg] parameter *)
              let n = Number.(int_literal ILitDec ~neg:false s) in
              Tconst (Constant.ConstInt n)
          | _ -> assert false (* TODO *) end
      | Tt.Tif (t1, t2, t3) ->
          Tif (term t1, term t2, term t3)
      | Tt.Tlet (vs, t1, t2) ->
          Tlet (ident_of_vsymbol vs, term t1, term t2)
      | Tt.Tcase (t, pat_term_list) ->
          let f_pair (pat, t) = pattern pat, term t in
          Tcase (term t, List.map f_pair pat_term_list)
      | Tt.Tquant (q, vs_list, trigger, t) ->
          let mk_trigger t = List.map term t in
          let trigger = List.map mk_trigger trigger in
          Tquant (quant q, List.map binder_of_vsymbol vs_list, trigger, term t)
      | Tt.Tbinop (op, t1, t2) ->
          Tbinop (term t1, binop op, term t2)
      | Tt.Tapp (ls, []) ->
          (* FIXME? is this the correct semantics for
               zero-arguments applications? *)
          Tident (Qident (ident_of_lsymbol ls))
      | Tt.Tapp (ls, term_list) when Tt.ls_equal ls Tt.fs_apply ->
          begin match term_list with
          | [fs; arg] -> Tapply (term fs, term arg)
          | _ -> assert false end
      | Tt.Tapp ({ls_name}, term_list) ->
          let loc = ls_name.I.id_loc in
          let term_list = List.map term term_list in
          let id_str = match query_syntax ls_name.id_str with
            | None   -> ls_name.id_str
            | Some s -> s in
          let id = mk_id id_str (location loc) in
          Tidapp (Qident id, term_list) in
    mk_term (t_node t.Tt.t_node)

end

let private_type = function
  | T.Private -> Private
  | T.Public  -> Public

let td_params (tvs, _) =
  Term.ident_of_tvsymbol tvs

(** Visibility of type declarations. An alias type cannot be private, so we
    check whether or not the GOSPEL type manifest is [None]. *)
let td_vis_from_manifest = function
  | None -> Private
  | Some _ -> Public

(** Convert a GOSPEL type definition into a Why3's Ptree [type_def]. If the
    GOSPEL type manifest is [None], then the type is defined via its
    specification fields. Otherwise, it is an alias type. *)
let td_def td_spec td_manifest =
  let td_def_of_ty_fields ty_fields =
    let field_of_lsymbol (ls, mut) =
      let id  = Term.ident_of_lsymbol ls in
      let pty = Term.ty Term.(Opt.get ls.Tt.ls_value) in
      mk_field id.id_loc id pty ~mut ~ghost:true in
    TDrecord (List.map field_of_lsymbol ty_fields) in
  match td_manifest with
  | None -> td_def_of_ty_fields td_spec.T.ty_fields
  | Some ty -> TDalias (Term.ty ty)

let type_decl (T.{td_ts = {ts_ident}; td_spec; td_manifest} as td) = T.{
  td_loc    = location td.td_loc;
  td_ident  = mk_id ts_ident.id_str (location td.td_loc);
  td_params = List.map td_params td.td_params;
  td_vis    = td_vis_from_manifest td_manifest;
  td_mut    = td_spec.ty_ephemeral;
  td_inv    = List.map Term.term td_spec.ty_invariants;
  td_wit    = [];
  td_def    = td_def td_spec td_manifest
}

let mk_expr expr_desc expr_loc =
  { expr_desc; expr_loc }

let mk_logic_decl ld_loc ld_ident ld_params ld_type ld_def =
  { ld_loc; ld_ident; ld_params; ld_type; ld_def }

let loc_of_vs vs = Term.(location vs.Tt.vs_name.I.id_loc)

let ident_of_lb_arg lb = Term.ident_of_vsymbol (T.vs_of_lb_arg lb)
let loc_of_lb_arg lb = loc_of_vs (T.vs_of_lb_arg lb)

(** Given the result type [sp_ret] of a function and a GOSPEL postcondition
    [post] (represented as a [term]), convert it into a Why3's Ptree
    postcondition of the form [Loc.position * (pattern * term)]. *)
let sp_post sp_ret post =
  let mk_post post =
    let pvar_of_lb_arg_list lb_arg_list =
      let mk_pvar lb = (* create a [Pvar] pattern out of a [Tt.lb_arg] *)
        Term.mk_pattern (Pvar (ident_of_lb_arg lb)) dummy_loc in
      List.map mk_pvar lb_arg_list in
    let pat = match pvar_of_lb_arg_list sp_ret with
      | [p] -> p
      | pl  -> Term.mk_pattern (Ptuple pl) dummy_loc in
    dummy_loc, [pat, Term.term post] in
  List.map mk_post post

open Term

(** Convert a GOSPEL exception postcondition of the form
    [(pattern * post) list Mxs.t] into a Why3's Ptree exceptional
    postcondition of the form
    [Loc.position * (qualid * (pattern * term) option) list]. *)
let sp_xpost xpost =
  let mk_xpost_list xs pat_post_list acc =
    let loc = xs.Ty.xs_ident.I.id_loc in
    let mk_xpost (pat, post) =
      let q = Qident (mk_id xs.Ty.xs_ident.I.id_str (location loc)) in
      let post = term post in
      q, Some (Term.pattern pat, post) in
    (location loc, List.map mk_xpost pat_post_list) :: acc in
  Ty.Mxs.fold mk_xpost_list xpost []

let sp_writes writes =
  List.map term writes

let spec val_spec = {
  (* TODO: for now, we ignore the [checks] preconditions *)
  sp_pre     = List.map (fun (t, _) -> term t) val_spec.T.sp_pre;
  sp_post    = sp_post val_spec.sp_ret val_spec.sp_post;
  sp_xpost   = sp_xpost val_spec.sp_xpost;
  sp_reads   = [];
  sp_writes  = sp_writes val_spec.sp_wr;
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
      let mk_str id_str = match query_syntax id_str with
        | None   -> id_str
        | Some s -> s in
      let rec longident loc = function
        | Gospel.Longident.Lident s ->
            Qident (mk_id (mk_str s) loc)
        | Gospel.Longident.Ldot (t, s) ->
            Qdot (longident loc t, mk_id (mk_str s) loc)
        | _ -> assert false (* TODO? *) in
      PTtyapp (longident (location loc) txt, List.map core_type ctl)
  | _ -> assert false (* TODO *)

(** Convert GOSPEL [val] declarations into Why3's Ptree [val] declarations. *)
let val_decl vd g =
  let rec flat_ptyp_arrow ct = match ct.Ot.ptyp_desc with
    | Ot.Ptyp_var _ | Ptyp_tuple _ | Ptyp_constr _ -> [ct]
    | Ot.Ptyp_arrow (_, t1, t2) ->
        begin match t1.ptyp_desc with
          | Ot.Ptyp_arrow (lbl, t11, t12) -> t1 :: flat_ptyp_arrow t2
          | _ -> flat_ptyp_arrow t1 @ flat_ptyp_arrow t2
        end
    | _ -> assert false (* TODO *) in
  let mk_param lb_arg ct =
    let loc = Term.(location (T.vs_of_lb_arg lb_arg).vs_name.I.id_loc) in
    let pty = core_type ct in
    let id  = Some (ident_of_lb_arg lb_arg) in
    let ghost, pty = match lb_arg with
      | Lnone vs | Lnamed vs ->
          false, pty
      | Lquestion vs ->
          false, PTtyapp (Qident (mk_id "option" loc), [pty])
      | Lghost vs ->
          true, pty in
    loc, id, ghost, pty in
  let mk_param_no_spec ct = let loc = location ct.Ot.ptyp_loc in
    loc, None, false, core_type ct in
  let param_list, ret, pat, mask, spec =
    let core_ty_list = flat_ptyp_arrow vd.T.vd_type in
    let core_ty_list, last = Lists.chop_last core_ty_list in
    let param_list, pat, mask, spec = match vd.T.vd_spec with
    | None -> let param_list = List.map mk_param_no_spec core_ty_list in
        (* when there is no specification, there is no pattern
           in the return tuple *)
        let pat = Term.mk_pattern Pwild (location last.Ot.ptyp_loc) in
        param_list, pat, Ity.MaskVisible, empty_spec
    | Some s -> let param_list = List.map2 mk_param s.T.sp_args core_ty_list in
        let mk_pat lb = let loc = loc_of_lb_arg lb in
          Term.mk_pattern (Pvar (ident_of_lb_arg lb)) loc in
        let mk_mask = function
          | T.Lnone  _ | T.Lquestion _ | T.Lnamed _ -> Ity.MaskVisible
          | T.Lghost _ -> Ity.MaskGhost in
        let lb_list = s.T.sp_ret in
        let pat_list  = List.map mk_pat lb_list in
        let mask_list = List.map mk_mask lb_list in
        let pat, mask = match pat_list, mask_list with
          | [], []   -> (* in this case, the return is of type unit *)
              Term.mk_pattern Pwild dummy_loc, Ity.MaskVisible
          | [p], [m] -> p, m
          | pl, ml   -> assert (List.length pl = List.length ml);
              let loc = location vd.T.vd_loc in (* TODO: better location? *)
              Term.mk_pattern (Ptuple pl) loc, Ity.MaskTuple ml in
        param_list, pat, mask, spec s in
    param_list, Some (core_type last), pat, mask, spec in
  let e_any  = Eany (param_list, Expr.RKnone, ret, pat, mask, spec) in
  let e_any  = mk_expr e_any (location vd.T.vd_loc) in
  let id_loc = location vd.vd_name.id_loc in
  Dlet (mk_id vd.vd_name.id_str id_loc, g, Expr.RKnone, e_any)

(** Convert GOSPEL logical declaration (function or predicate) into
     Why3's Ptree logical declaration. *)
let function_ (T.{fun_ls = Tt.{ls_name; ls_value}} as f) =
  let loc = location f.T.fun_loc in
  let id_loc = location ls_name.I.id_loc in
  let id = mk_id ls_name.I.id_str id_loc in
  let params = List.map param_of_vsymbol f.fun_params in
  let pty = Opt.map ty ls_value in
  let term = Opt.map term f.T.fun_def in
  Dlogic [mk_logic_decl loc id params pty term]

(** Convert GOSPEL axioms into Why3's Ptree axioms. *)
let axiom T.{ax_name; ax_term} =
  let id_loc = location ax_name.I.id_loc in
  let id = mk_id ax_name.I.id_str id_loc in
  let term = term ax_term in
  Dprop (Decl.Paxiom, id, term)

(** Convert GOSPEL exceptions into Why3's Ptree exceptions. *)
let exn T.{exn_constructor = {ext_ident; ext_xs}; exn_loc} =
  let id = mk_id ext_ident.id_str (location ext_ident.id_loc) in
  match ext_xs.Ty.xs_type with
  | Exn_tuple [{ty_node = Ty.Tyapp (ts, tyl)}] when Ty.is_ts_tuple ts ->
      Dexn (id, PTtuple (List.map ty tyl), Ity.MaskVisible)
  | Exn_tuple tyl ->
      Dexn (id, PTtuple (List.map ty tyl), Ity.MaskVisible)
  | Exn_record _ -> let loc = location exn_loc in
      Loc.errorm ~loc "Exceptions with record arguments is not supported."

(** Convert a GOSPEL module declaration into a Why3 scope. *)
let rec module_declaration T.{md_name; md_type; md_loc} =
  let loc = location md_loc in
  let id = mk_id md_name.I.id_str (location md_name.I.id_loc) in
  Gmodule (loc, id, module_type md_type)

and module_type mt = match mt.T.mt_desc with
  | T.Mod_ident _ (* of string list *) ->
      assert false
  | T.Mod_signature s ->
      List.flatten (signature s)
  | T.Mod_functor (arg_name, arg, body) ->
      let loc = location arg_name.I.id_loc in
      let id = mk_id arg_name.I.id_str loc in
      let body = module_type body in
      Gmodule (loc, id, module_type (Opt.get arg)) :: body
  | T.Mod_with _ (* of module_type * with_constraint list *) ->
      assert false
  | T.Mod_typeof _ (* of Oparsetree.module_expr *) ->
      assert false
  | T.Mod_extension _ (* of Oparsetree.extension *) ->
      assert false
  | T.Mod_alias _ (* of string list *) ->
      assert false

and signature_item i = match i.T.sig_desc with
  | T.Sig_val (vd, g) ->
      [Gdecl (val_decl vd g)]
  | T.Sig_type (_rec_flag, tdl, _gh) ->
      [Gdecl (Dtype (List.map type_decl tdl))]
  | T.Sig_typext _ (*  of Oparsetree.type_extension *) ->
      assert false (*TODO*)
  | T.Sig_module md ->
      [module_declaration md]
  | T.Sig_recmodule _ (* of module_declaration list *) ->
      assert false (*TODO*)
  | T.Sig_modtype _ (*  of module_type_declaration *) ->
      assert false (*TODO*)
  | T.Sig_exception exn_cstr ->
      [Gdecl (exn exn_cstr)]
  | T.Sig_open _ (* of open_description * ghost *) ->
      (* The GOSPEL standard library is opened by default. For now, we chose to
         ignore it and will make a work-around with a custom Why3 file. *)
      []
  | T.Sig_include _ (* of Oparsetree.include_description *) ->
      assert false (*TODO*)
  | T.Sig_class _ (* of Oparsetree.class_description list *) ->
      assert false (*TODO*)
  | T.Sig_class_type _ (* of Oparsetree.class_type_declaration list *) ->
      assert false (*TODO*)
  | T.Sig_attribute _ ->
      []
  | T.Sig_extension _ (* of Oparsetree.extension * Oparsetree.attributes *) ->
      assert false (*TODO*)
  | T.Sig_use _ (* of string *) ->
      assert false (*TODO*)
  | T.Sig_function f ->
      [Gdecl (function_ f)]
  | T.Sig_axiom ax ->
      [Gdecl (axiom ax)] (*TODO*)

and signature s = List.map signature_item s
