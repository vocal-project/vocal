(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Format
open Why3
open Pmodule
open Typing
open Ident
open Wstdlib
open Ptree
open Why3gospel_trans

let debug = ref true

let print_modules = Debug.lookup_flag "print_modules"

let mk_id ?(loc=Loc.dummy_position) name =
  { id_str = name; id_ats = []; id_loc = loc; }

let use ?(import=false) q =
  let id = match q with Qident id | Qdot (_, id) -> id in
  let loc = id.id_loc in
  Typing.open_scope loc id;
  let use_import = Duseimport (loc, import, [q, None]) in
  Typing.add_decl loc use_import;
  Typing.close_scope loc ~import

let use_ocaml loc =
  use ~import:true (Qdot (Qident (mk_id ~loc "array"), mk_id ~loc "Array"))

let read_file file nm c =
  let lb = Lexing.from_channel c in
  Gospel.Location.init lb file;
  Gospel.Parser_frontend.(parse_gospel (parse_ocaml_lb lb) nm)

(* TODO
(* extract additional uses and vals from file.mli.why3, if any *)
let extract_use = function Use q -> Some q | _ -> None

let extract_vals m = function
  | Val (_,_,id,pty) -> Mstr.add id.id_str pty m
  | _ -> m

let read_extra_file file =
  let why3_file = file ^ ".why3" in
  if Sys.file_exists why3_file then begin
    let c = open_in why3_file in
    let f = read_file why3_file c in
    close_in c;
    Lists.map_filter extract_use f,
    List.fold_left extract_vals Mstr.empty f
  end else
    [], Mstr.empty
*)

(* TODO equivalent clauses
let print_equiv file dl =
  let f_equiv = let file = file ^ ".equiv" in
    if Sys.file_exists file then begin
      let backup = file ^ ".bak" in Sys.rename file backup end;
    open_out file in
  let fmt_equiv = formatter_of_out_channel f_equiv in
  let print_args fmt = function
    | Lnone id -> fprintf fmt "%s" id.id_str
    | Lquestion id -> fprintf fmt "?%s" id.id_str
    | Lnamed id -> fprintf fmt "~%s" id.id_str
    | Lghost _ -> assert false in
  let print_decl fmt = function
    | Ddecl _ | Duse _ | Dmodule _ -> () (* FIXME: equiv inside sub-module? *)
    | Dequivalent (fid, argsl, body) ->
      fprintf fmt "let %s @[%a@]@ =@;<1 2>%s@\n@\n"
        fid.id_str (Pp.print_list Pp.space print_args) argsl body in
  List.iter (fun x -> print_decl fmt_equiv x) dl;
  fprintf fmt_equiv "@.";
  close_out f_equiv

let filter_equiv =
  let mk_equiv acc = function Dequivalent _ as e -> e::acc | _ -> acc in
  List.fold_left mk_equiv []
*)

let type_check name nm sigs =
  let md = Gospel.Tmodule.init_muc name in
  let penv = Gospel.Typing.penv [] (Gospel.Utils.Sstr.singleton nm) in
  let md = List.fold_left (Gospel.Typing.type_sig_item penv) md sigs in
  Gospel.Tmodule.wrap_up_muc md

let use_gospel =
  let gospel = Qdot (Qident (mk_id "gospel"), mk_id "Gospel") in
  let array =
    Qdot (Qdot (Qident (mk_id "mach"), mk_id "array"), mk_id "Array63") in
  let seq = Qdot (Qident (mk_id "seq"), mk_id "Seq") in
  let use_gospel = Duseimport (Loc.dummy_position, false, [gospel, None]) in
  let use_array = Duseimport (Loc.dummy_position, false, [array, None]) in
  let use_seq = Duseimport (Loc.dummy_position, false, [seq, None]) in
  [Gdecl use_gospel; Gdecl use_array; Gdecl use_seq]

let read_channel env path file c =
  if !debug then eprintf "reading file '%s'@." file;
  (* let extra_uses, extra_vals = read_extra_file file in *)
  let nm =
    let f = Filename.basename file in
    String.capitalize_ascii (Filename.chop_extension f) in
  let f = read_file file nm c in
  let f = type_check file nm f in
  let f = Why3gospel_trans.signature (*extra_vals*) f.fl_sigs in
  open_file env path;
  let id = mk_id "Sig" in
  open_module id;
  use_ocaml id.id_loc;
  (* List.iter use extra_uses; *)
  let rec add_decl = function
    | Gdecl d -> Typing.add_decl Loc.dummy_position d;
    | Gmodule (id, dl) ->
       Typing.open_scope id.id_loc id;
       List.iter add_decl dl;
       Typing.close_scope ~import:false id.id_loc in
  let f = use_gospel @ List.flatten f in
  (* For debugging only: *)
  (* List.iter (fun d -> match d with
   *     | Gdecl d -> Format.eprintf "%a@." Mlw_printer.pp_decl d
   *     | _ -> assert false) f; *)
  List.iter add_decl f;
  close_module Loc.dummy_position;
  let mm = close_file () in
  (* TODO *)
  (* let f = filter_equiv f in
   * if f <> [] then print_equiv file f; *)
  if Debug.test_flag print_modules then begin
    let print_m _ m = Format.eprintf "%a@\n@." print_module m in
    let add_m _ m mm = Mid.add m.mod_theory.Theory.th_name m mm in
    Mid.iter print_m (Mstr.fold add_m mm Mid.empty)
  end;
  mm

let () =
  Env.register_format mlw_language "gospel" ["mli"] read_channel
    ~desc:"GOSPEL format"
