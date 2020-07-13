open Gospel
open Tterm
open Ttypes

type path = string list

val reduce_path : path -> path -> path

module Hls : Hashtbl.S with type key = lsymbol
module Hts : Hashtbl.S with type key = tysymbol
module Hxs : Hashtbl.S with type key = xsymbol

type info = private {
  info_ls   : path Hls.t;
  info_ts   : path Hts.t;
  info_xs   : path Hxs.t;
  info_path : path;
}

val empty_info : info

val find_ls : info -> lsymbol  -> path
val find_ts : info -> tysymbol -> path
val find_xs : info -> xsymbol  -> path

val add_ls : info -> lsymbol  -> unit
val add_ts : info -> tysymbol -> unit
val add_xs : info -> xsymbol  -> unit

val update_path : info -> string -> info
