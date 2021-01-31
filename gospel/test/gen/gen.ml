let print_rule file =
  if Filename.extension file = ".mli" then
    let file = Filename.chop_extension file in
    Printf.printf
      {|
(rule
 (target %s.mli.pps)
 (action
   (with-stdout-to %%{target}
     (run gospel_pps %%{dep:%s.mli}))))

(rule
 (target %s.mli.output)
 (action
    (with-outputs-to %%{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %%{dep:%s.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %%{dep:%s.mli.expected} %%{dep:%s.mli.output})))
|}
      file file file file file file

let () =
  let files = Filename.current_dir_name |> Sys.readdir in
  Array.sort String.compare files;
  Array.iter print_rule files
