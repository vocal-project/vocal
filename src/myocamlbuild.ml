open Ocamlbuild_plugin
open Command

let () =
  dispatch (function After_rules ->
    (* -noassert (if enabled by tag) *)
    flag ["ocaml"; "compile"; "noassert"] (A "-noassert");
    (* -unsafe (if enabled by tag) *)
    flag ["ocaml"; "compile"; "unsafe"] (A "-unsafe");
  | _ -> ()
  )
