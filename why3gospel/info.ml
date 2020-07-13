module TS = struct
  include Gospel.Ttypes.Ts
  let hash = (Hashtbl.hash : Gospel.Ttypes.tysymbol -> int)
end

module XS = struct
  include Gospel.Ttypes.Xs
  let hash = (Hashtbl.hash : Gospel.Ttypes.xsymbol -> int)
end

module Hls = Hashtbl.Make(Gospel.Tterm.LS)
module Hts = Hashtbl.Make(TS)
module Hxs = Hashtbl.Make(XS)

type path = string list

let reduce_path info_path curr_path =
  let rec loop info_path curr_path = match info_path, curr_path with
    | p, [] -> p
    | ip :: rip, cp :: rcp -> assert (ip = cp); loop rip rcp
    | _ -> assert false (* I think this covers all the possible cases *) in
  loop info_path curr_path

type info = {
  info_ls   : path Hls.t;
  info_ts   : path Hts.t;
  info_xs   : path Hxs.t;
  info_path : path;
}

let empty_info = {
  info_ls   = Hls.create 16;
  info_ts   = Hts.create 16;
  info_xs   = Hxs.create 16;
  info_path = [];
}

let find_ls info ls = Hls.find info.info_ls ls
let find_ts info ts = Hts.find info.info_ts ts
let find_xs info xs = Hxs.find info.info_xs xs

let add_ls  info ls = Hls.add info.info_ls ls info.info_path
let add_ts  info ts = Hts.add info.info_ts ts info.info_path
let add_xs  info xs = Hxs.add info.info_xs xs info.info_path

let update_path info str = { info with info_path = str :: info.info_path }
