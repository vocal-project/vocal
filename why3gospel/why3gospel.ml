
open Format
open Why3
open Ptree
open Pmodule
open Typing
open Ident
open Wstdlib

let debug = ref true

let print_modules = Debug.lookup_flag "print_modules"

let mk_id ?(loc=Loc.dummy_position) name =
  { id_str = name; id_ats = []; id_loc = loc; }

let use ?(import=false) q =
  let id = match q with Qident id | Qdot (_, id) -> id in
  let loc = id.id_loc in
  Typing.open_scope loc id;
  Typing.add_decl loc (Duse q);
  Typing.close_scope loc ~import

let use_ocaml loc =
  use ~import:true (Qdot (Qident (mk_id ~loc "ocaml"), mk_id ~loc "OCaml"))

let read_file file c =
  let lb = Lexing.from_channel c in
  Location.init lb file;
  Gospel.Parser_frontend.(parse_gospel (parse_ocaml_lb lb))

let read_channel env path file _c =
  if !debug then eprintf "reading file '%s'@." file;
  open_file env path;
  let id = mk_id "Sig" in
  open_module id;
  use_ocaml id.id_loc;
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
