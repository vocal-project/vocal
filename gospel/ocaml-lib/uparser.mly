(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

%{
  open Identifier
  open Uast
  open Uast_utils

  let rev_fspec s = {
      fun_req     = List.rev s.fun_req;
      fun_ens     = List.rev s.fun_ens;
      fun_variant = List.rev s.fun_variant;
      fun_coer    = s.fun_coer;
    }

  let empty_fspec = {
      fun_req     = [];
      fun_ens     = [];
      fun_variant = [];
      fun_coer    = false;
    }

  let rev_vspec s = {
    sp_hd_ret  = s.sp_hd_ret;
    sp_hd_nm  = s.sp_hd_nm;
    sp_hd_args = s.sp_hd_args;
    sp_pre     = List.rev s.sp_pre;
    sp_post    = List.rev s.sp_post;
    sp_xpost   = List.rev s.sp_xpost;
    sp_reads   = List.rev s.sp_reads;
    sp_writes  = List.rev s.sp_writes;
    sp_consumes= List.rev s.sp_consumes;
    sp_alias   = List.rev s.sp_alias;
    sp_variant = List.rev s.sp_variant;
    sp_diverge = s.sp_diverge;
    sp_equiv   = List.rev s.sp_equiv;
  }

  let empty_vspec = {
    sp_hd_ret  = [];
    sp_hd_nm   = mk_pid "" Lexing.dummy_pos Lexing.dummy_pos;
    sp_hd_args = [];
    sp_pre     = [];
    sp_post    = [];
    sp_xpost   = [];
    sp_reads   = [];
    sp_writes  = [];
    sp_consumes= [];
    sp_alias   = [];
    sp_variant = [];
    sp_diverge = false;
    sp_equiv   = [];
  }

  let rev_tspec s = {
      ty_ephemeral = s.ty_ephemeral;
      ty_field     = List.rev s.ty_field;
      ty_invariant = List.rev s.ty_invariant;
    }

  let empty_tspec = {
      ty_ephemeral = false;
      ty_field     = [];
      ty_invariant = [];
    }

%}

(* Tokens *)

%token <string> LIDENT UIDENT
%token <string> INTEGER
%token <string> OP1 OP2 OP3 OP4 OPPREF
%token <string> FLOAT
%token <string> QUOTE_LIDENT
%token <string> BACKQUOTE_LIDENT
%token <string> STRING
%token <string> ATTRIBUTE

(* Spec Tokens *)

%token REQUIRES ENSURES CONSUMES VARIANT

(* keywords *)

%token AXIOM LEMMA GOAL (* CONSTANT *)
%token EPHEMERAL ELSE EXISTS FALSE FORALL FUNCTION FUN
%token REC
%token INVARIANT
%token COERCION
%token IF IN
%token OLD NOT RAISES (* READS *)
%token THEN TRUE TYPE OPEN VAL MODIFIES EQUIVALENT CHECKS DIVERGES

%token AS ANDW
%token LET MATCH PREDICATE
%token WITH

(* symbols *)

%token AND AMPAMP ARROW BAR BARBAR COLON COLONCOLON COMMA DOT DOTDOT
%token EOF EQUAL COLONEQUAL
%token MUTABLE MODEL
%token LARROW LRARROW LEFTBRC LEFTBRCCOLON LEFTPAR LEFTBRCRIGHTBRC
%token LEFTSQ LTGT OR QUESTION RIGHTBRC COLONRIGHTBRC RIGHTPAR RIGHTSQ SEMICOLON
%token LEFTSQRIGHTSQ
%token STAR TILDA UNDERSCORE

(* priorities *)

%nonassoc IN
%nonassoc DOT ELSE
%nonassoc prec_named
%right COLON

%right ARROW LRARROW
%right OR BARBAR
%right AND AMPAMP
%nonassoc NOT
%right EQUAL LTGT OP1
%right COLONCOLON
%nonassoc OLD
%left OP2
%left OP3 STAR
%left OP4
%left BACKQUOTE_LIDENT
%nonassoc prec_prefix_op
%nonassoc LEFTSQ
%nonassoc OPPREF

%left BAR

%start <Uast.spec> spec_init
%start <Uast.loop_spec> loop_spec

%%

spec_init:
| type_spec EOF     { Stype (rev_tspec $1, mk_loc $startpos $endpos) }
| val_spec EOF      { Sval ($1, mk_loc $startpos $endpos) }
| func EOF          { Sfunction ($1, mk_loc $startpos $endpos)}
| constraint_spec EOF
     { Sconstraint ($1, mk_loc $startpos $endpos) }
(* | func_spec EOF      { Sfunc_spec (rev_fspec $1, mk_loc $startpos $endpos)} *)
| prop EOF          { Sprop ($1, mk_loc $startpos $endpos)}
| VAL               { raise Ghost_decl }
| TYPE              { raise Ghost_decl }
| OPEN              { raise Ghost_decl }
;

prop:
| AXIOM id=lident COLON t=term
  { {prop_name = id; prop_term = t;
     prop_loc = mk_loc $startpos $endpos; prop_kind = Paxiom} }
| LEMMA id=lident COLON t=term
  { {prop_name = id; prop_term = t;
     prop_loc = mk_loc $startpos $endpos; prop_kind = Plemma} }
;

func:
| FUNCTION r=REC? fname=func_name ps=params? COLON ty=typ
    def=preceded(EQUAL, term)? spec=func_spec?
  { let ps = match ps with | None -> [] | Some ps -> ps in
    let spec = match spec with
        None -> empty_fspec | Some spec -> rev_fspec spec in
    { fun_name = fname; fun_rec = Utils.opt2bool r; fun_type = Some ty;
      fun_params = ps; fun_def = def; fun_spec = spec;
      fun_loc = mk_loc $startpos $endpos} }
| PREDICATE r=REC? fname=func_name ps=params
    def=preceded(EQUAL, term)? spec=func_spec?
  { let spec = match spec with
        None -> empty_fspec | Some spec -> rev_fspec spec in
    { fun_name = fname; fun_rec = Utils.opt2bool r; fun_type = None;
      fun_params = ps; fun_def = def; fun_spec = spec ;
      fun_loc = mk_loc $startpos $endpos} }
;

func_name:
| lident_rich {$1}
| LEFTPAR LEFTBRCRIGHTBRC RIGHTPAR
    { mk_pid (mixfix "{}") $startpos $endpos }
| LEFTPAR LEFTBRCCOLON UNDERSCORE COLONRIGHTBRC RIGHTPAR
    { mk_pid (mixfix "{:_:}") $startpos $endpos }

func_spec:
| t=requires { {empty_fspec with fun_req = [t]} }
| t=ensures  { {empty_fspec with fun_ens = [t]} }
| t=variant  { {empty_fspec with fun_variant = [t]} }
| COERCION   { {empty_fspec with fun_coer = true}}
| bd=func_spec t=requires
    { {bd with fun_req = t :: bd.fun_req} }
| bd=func_spec t=ensures
    { {bd with fun_ens = t :: bd.fun_ens} }
| bd=func_spec t=variant
    { {bd with fun_variant = t :: bd.fun_variant} }
| bd=func_spec COERCION
    { {bd with fun_coer = true} }
;

variant:
| VARIANT t=term {t}
;

requires:
| REQUIRES t=term {t}
;

ensures:
| ENSURES t=term {t}
;

type_spec:
| EPHEMERAL
    { {empty_tspec with ty_ephemeral = true} }
| field=type_spec_model
    { {empty_tspec with ty_field = [field]} }
| inv=type_spec_invariant
    { {empty_tspec with ty_invariant = [inv]} }
| ts=type_spec EPHEMERAL
  { {ts with ty_ephemeral = true} }
| ts=type_spec field=type_spec_model
  { {ts with ty_field = field :: ts.ty_field} }
| ts=type_spec inv=type_spec_invariant
  { {ts with ty_invariant = inv :: ts.ty_invariant} }
;

type_spec_model:
| mut = boption(MUTABLE) MODEL id = lident_rich COLON ty=typ
  { { f_preid = id; f_mutable = mut;
      f_pty = ty; f_loc = mk_loc $startpos(mut) $endpos(ty) }}

type_spec_invariant:
| INVARIANT inv=term
  { inv }

val_spec:
| hd=val_spec_header bd=val_spec_body
  { let bd = rev_vspec bd in
    let (r,f,a) = hd in
    { bd with sp_hd_ret  = r;
      sp_hd_nm  = f; sp_hd_args = a;}
  }
| bd = val_spec_body
  { rev_vspec bd }
;

val_spec_header:
| ret=ret_name nm=lident_rich args=fun_arg*
    { ret, nm, args }
| nm=lident_rich args=fun_arg*
    { [], nm, args }
;

val_spec_body:
| (* epsilon *)
  { empty_vspec }
| bd=val_spec_body DIVERGES
  { {bd with sp_diverge = true} }
| bd=val_spec_body VARIANT v=separated_list(COMMA, term)
  { {bd with sp_diverge = false; sp_variant = v} }
| bd=val_spec_body MODIFIES wr=separated_list(COMMA, term)
    { { bd with sp_writes = wr @ bd.sp_writes } }
| bd=val_spec_body CONSUMES cs=separated_list(COMMA, term)
    { { bd with sp_consumes = cs @ bd.sp_consumes } }
| bd=val_spec_body t=requires
    { { bd with sp_pre = (t,false) :: bd.sp_pre } }
| bd=val_spec_body CHECKS t = term
    { { bd with sp_pre = (t,true) :: bd.sp_pre } }
| bd=val_spec_body t=ensures
    { { bd with sp_post = t :: bd.sp_post} }
| bd=val_spec_body RAISES r=bar_list1(raises) (* raises_list *)
    { let xp = mk_loc $startpos(r) $endpos(r), r in
      { bd with sp_xpost = xp :: bd.sp_xpost } }
| bd=val_spec_body EQUIVALENT e=STRING
    { { bd with sp_equiv = e :: bd.sp_equiv} }
;

loop_spec: _loop_spec EOF
  { let inv, var = $1 in
    { loop_invariant = inv; loop_variant = var } }

_loop_spec:
| (* epsilon *)
    { [], [] }
| INVARIANT t = term _loop_spec
    { let inv, var = $3 in t :: inv, var }
| VARIANT   t = term _loop_spec
    { let inv, var = $3 in inv, t :: var }

fun_arg:
| LEFTPAR RIGHTPAR
   { Lnone (create_pid "()" []  (mk_loc $startpos $endpos)) }
| lident
   { Lnone $1 }
| TILDA lident
   { Lnamed $2 }
| QUESTION lident
   { Lquestion $2 }
| LEFTSQ id=lident COLON ty=typ RIGHTSQ
   { Lghost (id, ty) }
;

ret_value:
| lident
   { Lnone $1 }
| LEFTSQ id=lident COLON ty=typ RIGHTSQ
   { Lghost (id, ty) }

ret_name:
| LEFTPAR comma_list(ret_value)  RIGHTPAR EQUAL
    { $2 }
| comma_list(ret_value)  EQUAL
    { $1 }
;

raises:
| q=uqualid ARROW t=term
    { q, Some (mk_pat (Ptuple []) $startpos(q) $endpos(q), t) }
| q=uqualid p=pat_arg ARROW t=term
    { q, Some (p, t) }
| q=uqualid
    { q, None}
;

params:
| param  { $1 }
| param params { $1 @ $2 }
;

param:
| LEFTPAR params=lident+ COLON t=ty RIGHTPAR
    { List.map (fun x -> mk_loc $startpos $endpos, x, t) params }
;

cast:
| COLON ty_arg  { $2 }
;

constraint_spec:
| WITH and_list(single_constraint) { $2 }

single_constraint:
| FUNCTION idl = qualid EQUAL idr = qualid
    { CFunctionShare (idl, idr) }
| FUNCTION idl = qualid COLONEQUAL idr = qualid
    { CFunctionDestr (idl, idr) }
| PREDICATE idl = qualid EQUAL idr = qualid
    { CPredicateShare (idl, idr) }
| PREDICATE idl = qualid COLONEQUAL idr = qualid
    { CPredicateDestr (idl, idr) }
| GOAL q = qualid
    { CGoal q }
| AXIOM q = qualid
    { CAxiom q }

term: t = mk_term(term_) { t }
;

term_:
| term_arg_
    { $1 }
| NOT term
    { Tnot $2 }
| OLD term
    { Told $2 }
| prefix_op term %prec prec_prefix_op
    { Tidapp (Qpreid $1, [$2]) }
| l = term ; o = bin_op ; r = term
    { Tbinop (l, o, r) }
| l = term ; o = infix_op_1 ; r = term
    { Tinfix (l, o, r) }
| l = term ; o = infix_op_234 ; r = term
    { Tidapp (Qpreid o, [l; r]) }
| l = term ; COLONCOLON ; r = term
    { Tidapp (Qpreid (mk_pid (infix "::") $startpos $endpos), [l; r]) }
| l = term ; o = BACKQUOTE_LIDENT ; r = term
    { let id = mk_pid o $startpos $endpos in
      Tidapp (Qpreid id, [l; r]) }
| term_arg located(term_arg)+
    { let join f (a,_,e) = mk_term (Tapply (f,a)) $startpos e in
      (List.fold_left join $1 $2).term_desc }
| IF term THEN term ELSE term
    { Tif ($2, $4, $6) }
| LET pattern EQUAL term IN term
    { let cast ty = { $4 with term_desc = Tcast ($4, ty) } in
      let pat, def = match $2.pat_desc with
        | Ptuple [] -> { $2 with pat_desc = Pwild }, cast (PTtuple [])
        | Pcast ({pat_desc = (Pvar _|Pwild)} as p, ty) -> p, cast ty
        | _ -> $2, $4 in
      match pat.pat_desc with
      | Pvar id -> Tlet (id, def, $6)
      | Pwild -> Tlet (id_anonymous pat.pat_loc, def, $6)
      | _ -> Tcase (def, [pat, $6]) }
| LET attrs(lident_op_id) EQUAL term IN term
    { Tlet ($2, $4, $6) }
| MATCH term WITH match_cases(term)
    { Tcase ($2, $4) }
| MATCH comma_list2(term) WITH match_cases(term)
    { Tcase (mk_term (Ttuple $2) $startpos($2) $endpos($2), $4) }
| quant comma_list1(quant_vars) triggers DOT term
    { Tquant ($1, List.concat $2, $3, $5) }
| FUN args = quant_vars ARROW t = term
    { Tquant (Tlambda, args, [], t) }
| attr term %prec prec_named
    { Tattr ($1, $2) }
| term cast
    { Tcast ($1, $2) }
;

field_list1(X):
| fl = semicolon_list1(term_rec_field(X)) { fl }
;

term_rec_field(X):
| separated_pair(lqualid, EQUAL, X) { $1 }
| lqualid { let t = {term_desc = Tpreid $1;
                     term_loc = loc_of_qualid $1} in
            ($1,t)
          }
;

match_cases(X):
| cl = bar_list1(separated_pair(pattern, ARROW, X)) { cl }
;

quant_vars:
| binder_var+ cast? { List.map (fun id -> id, $2) $1 }
;

triggers:
| (* epsilon *)                                                 { [] }
| LEFTSQ separated_nonempty_list(BAR,comma_list1(term)) RIGHTSQ { $2 }
;

attrs(X): X attr* { pid_add_lab $1 $2 }
;

attr:
| ATTRIBUTE    { $1 }
;

term_arg: mk_term(term_arg_) { $1 };
term_dot: mk_term(term_dot_) { $1 }
;

term_arg_:
| qualid                    { Tpreid $1 }
| numeral                   { Tconst $1 }
| TRUE                      { Ttrue }
| FALSE                     { Tfalse }
| o = oppref ; a = term_arg { Tidapp (Qpreid o, [a]) }
| term_sub_                 { $1 }
;

term_dot_:
| lqualid                   { Tpreid $1 }
| o = oppref ; a = term_dot { Tidapp (Qpreid o, [a]) }
| term_sub_                 { $1 }
;

term_block_:
| LEFTPAR t=term RIGHTPAR                           { t.term_desc }
| LEFTPAR RIGHTPAR                                  { Ttuple [] }
| LEFTSQRIGHTSQ
    { Tpreid (Qpreid (mk_pid "[]"  $startpos $endpos)) }
| LEFTBRC field_list1(term) RIGHTBRC                { Trecord $2 }
| LEFTBRC term_arg WITH field_list1(term) RIGHTBRC  { Tupdate ($2,$4) }
| LEFTBRCRIGHTBRC
    { Tpreid (Qpreid (mk_pid (mixfix "{}") $startpos $endpos)) }
| LEFTBRCCOLON t=term COLONRIGHTBRC
    { let id = Qpreid (mk_pid (mixfix "{:_:}") $startpos $endpos) in
      Tidapp (id, [t]) }
;

term_sub_:
| term_block_                                       { $1 }
| uqualid DOT mk_term(term_block_)                  { Tscope ($1, $3) }
| term_dot DOT lqualid_rich                         { Tidapp ($3,[$1]) }
| term_arg LEFTSQ term RIGHTSQ
    { Tidapp (get_op $startpos($2) $endpos($2), [$1;$3]) }
| term_arg LEFTSQ term LARROW term RIGHTSQ
    { Tidapp (set_op $startpos($2) $endpos($2), [$1;$3;$5]) }
| term_arg LEFTSQ term DOTDOT term RIGHTSQ
    { Tidapp (sub_op $startpos($2) $endpos($2), [$1;$3;$5]) }
| term_arg LEFTSQ term DOTDOT RIGHTSQ
    { Tidapp (above_op $startpos($2) $endpos($2), [$1;$3]) }
| term_arg LEFTSQ DOTDOT term RIGHTSQ
    { Tidapp (below_op $startpos($2) $endpos($2), [$1;$4]) }
| LEFTPAR comma_list2(term) RIGHTPAR                { Ttuple $2 }
| term_dot DOT LEFTPAR term RIGHTPAR
    { Tidapp (array_get $startpos($2) $endpos($2), [$1; $4]) }
| t1=term_dot DOT LEFTPAR t2=term LARROW t3=term  RIGHTPAR
    { Tidapp (set_op $startpos($2) $endpos($2), [t1;t2;t3]) }
;

%inline bin_op:
| ARROW   { Timplies }
| LRARROW { Tiff }
| OR      { Tor }
| BARBAR  { Tor_asym }
| AND     { Tand }
| AMPAMP  { Tand_asym }
;

quant:
| FORALL  { Tforall }
| EXISTS  { Texists }
;

numeral:
| INTEGER { Oasttypes.Pconst_integer ($1,None) }
| FLOAT   { Oasttypes.Pconst_float ($1, None) }
;

binder_var:
| attrs(lident)  { $1 }
;

mk_expr(X): d = X { mk_expr d $startpos $endpos }
;

typ:
| ty_arg
    { $1 }
| id=lident COLON aty=typ ARROW rty=typ
    { PTarrow (Lnamed id, aty, rty) }
| QUESTION id=lident COLON aty=typ ARROW rty=typ
    { PTarrow (Lquestion id, aty, rty) }
| typ ARROW typ
    { let l = mk_loc $startpos($1) $endpos($2) in
      PTarrow (Lnone (id_anonymous l), $1, $3) }
| ty_arg STAR ty_tuple
    { PTtuple ($1 :: $3) }
;

ty_tuple:
| ty_arg  (* %prec prec_ty_arg *)
    { [$1] }
| ty_arg STAR ty_tuple
    { $1 :: $3 }
;

ty_arg:
| lqualid
    { PTtyapp ($1, []) }
| quote_lident
    { PTtyvar $1 }
| LEFTPAR typ RIGHTPAR
    { $2 }
| ty_arg lqualid
    { PTtyapp ($2, [$1]) }
| LEFTPAR typ COMMA separated_nonempty_list(COMMA, typ)  RIGHTPAR id=lqualid
    { PTtyapp (id, $2::$4) }
;

%inline ty:
| typ {$1}
;

mk_term(X): d = X { mk_term d $startpos $endpos }
;

(* Patterns *)

mk_pat(X): X { mk_pat $1 $startpos $endpos }
;

pattern: mk_pat(pattern_) { $1 };
pat_arg: mk_pat(pat_arg_) { $1 }
;

pattern_:
| pat_conj_                             { $1 }
| mk_pat(pat_conj_) BAR pattern         { Por ($1,$3) }
;

pat_conj_:
| pat_uni_                              { $1 }
| comma_list2(mk_pat(pat_uni_))         { Ptuple $1 }
;

pat_uni_:
| pat_arg_                              { $1 }
| pat_arg COLONCOLON pat_arg
    { Papp (Qpreid (mk_pid (infix "::") $startpos $endpos),[$1;$3]) }
| uqualid pat_arg+                      { Papp ($1,$2) }
| mk_pat(pat_uni_) AS attrs(lident)
                                        { Pas ($1,$3) }
| mk_pat(pat_uni_) cast                 { Pcast ($1, $2) }
;

pat_arg_:
| pat_arg_shared_                       { $1 }
| attrs(lident)                         { Pvar $1 }
;

pat_arg_shared_:
| UNDERSCORE                            { Pwild }
| uqualid                               { Papp ($1,[]) }
| LEFTPAR RIGHTPAR                      { Ptuple [] }
| LEFTSQRIGHTSQ
  { Papp (Qpreid (mk_pid "[]"  $startpos $endpos), []) }
| LEFTPAR pattern_ RIGHTPAR             { $2 }
| LEFTBRC field_pattern(pattern) RIGHTBRC { Prec $2 }
;

field_pattern(X):
| fl = semicolon_list1(pattern_rec_field(X)) { fl }
;

pattern_rec_field(X):
| separated_pair(lqualid, EQUAL, X) { $1 }
| lqualid { let p = {pat_desc = Pvar (qualid_preid $1);
                     pat_loc = loc_of_qualid $1} in
            ($1,p)
          }
;

(* Symbolic operation names *)

op_symbol:
| OP1  { $1 }
| OP2  { $1 }
| OP3  { $1 }
| OP4  { $1 }
| STAR { "*" }
;

%inline oppref:
| o = OPPREF { mk_pid (prefix o)  $startpos $endpos }
;

prefix_op:
| op_symbol { mk_pid (prefix $1)  $startpos $endpos }
;

%inline infix_op_1:
| o = OP1   { mk_pid (infix o)    $startpos $endpos }
| EQUAL     { mk_pid (infix "=")  $startpos $endpos }
| LTGT      { mk_pid (infix "<>") $startpos $endpos }
%inline infix_op_234:
| o = OP2   { mk_pid (infix o)    $startpos $endpos }
| o = OP3   { mk_pid (infix o)    $startpos $endpos }
| STAR      { mk_pid (infix "*")   $startpos $endpos }
| o = OP4   { mk_pid (infix o)    $startpos $endpos }
;

(* Idents *)

lident:
| LIDENT        { mk_pid $1 $startpos $endpos }
;

uident:
| UIDENT        { mk_pid $1 $startpos $endpos }
;

quote_lident:
| QUOTE_LIDENT  { mk_pid $1 $startpos $endpos }
;

(* ident:
 * | uident        { $1 }
 * | lident        { $1 }
 * ; *)

ident_rich:
| uident        { $1 }
| lident_rich   { $1 }
;

lident_rich:
| lident        { $1 }
| lident_op_id  { $1 }
;

lident_op_id:
| LEFTPAR lident_op RIGHTPAR  { mk_pid $2 $startpos $endpos }
;

lident_op:
| op_symbol                                   { infix $1        }
| op_symbol UNDERSCORE                        { prefix $1       }
| EQUAL                                       { infix "="       }
| OPPREF UNDERSCORE?                          { prefix $1       }
| DOT LEFTPAR RIGHTPAR                        { mixfix ".()"    }
| DOT LEFTPAR LARROW RIGHTPAR                 { mixfix ".(<-)"  }
| LEFTSQ UNDERSCORE RIGHTSQ                   { mixfix "[_]"     }
| LEFTSQ LARROW RIGHTSQ                       { mixfix "[<-]"   }
| LEFTSQ UNDERSCORE DOTDOT UNDERSCORE RIGHTSQ { mixfix "[_.._]" }
| LEFTSQ            DOTDOT UNDERSCORE RIGHTSQ { mixfix "[.._]"  }
| LEFTSQ UNDERSCORE DOTDOT            RIGHTSQ { mixfix "[_..]"  }
;

(* Qualified idents *)

(* any_qualid:
 * | ident                { Qpreid $1 }
 * | any_qualid DOT ident { Qdot ($1, $3) }
 * ; *)

qualid:
| ident_rich              { Qpreid $1 }
| uqualid DOT ident_rich  { Qdot ($1, $3) }
;

lqualid_rich:
| lident_rich             { Qpreid $1 }
| uqualid DOT lident_rich { Qdot ($1, $3) }
;

lqualid:
| lident              { Qpreid $1 }
| uqualid DOT lident  { Qdot ($1, $3) }
;

uqualid:
| uident              { Qpreid $1 }
| uqualid DOT uident  { Qdot ($1, $3) }
;

(* Miscellaneous *)

comma_list(X):
| separated_nonempty_list(COMMA, X) { $1 }
;

bar_list1(X):
| ioption(BAR) ; xl = separated_nonempty_list(BAR, X) { xl }
;

with_list1(X):
| separated_nonempty_list(WITH, X)  { $1 }
;

comma_list2(X):
| X COMMA comma_list1(X) { $1 :: $3 }
;

comma_list1(X):
| separated_nonempty_list(COMMA, X) { $1 }
;

semicolon_list1(X):
| x = X ; ioption(SEMICOLON)                  { [x] }
| x = X ; SEMICOLON ; xl = semicolon_list1(X) { x :: xl }
;

star_list2(X):
| X STAR separated_nonempty_list(STAR, X) { $1 :: $3 }
;

and_list(X):
| separated_nonempty_list (ANDW, X) { $1 }
;

located(X): X { $1, $startpos, $endpos }
;
