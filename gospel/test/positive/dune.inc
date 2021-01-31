
(rule
 (target FM19.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:FM19.mli}))))

(rule
 (target FM19.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:FM19.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:FM19.mli.expected} %{dep:FM19.mli.output})))

(rule
 (target abstract_functions.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:abstract_functions.mli}))))

(rule
 (target abstract_functions.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:abstract_functions.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:abstract_functions.mli.expected} %{dep:abstract_functions.mli.output})))

(rule
 (target basic_functions_axioms.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:basic_functions_axioms.mli}))))

(rule
 (target basic_functions_axioms.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:basic_functions_axioms.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:basic_functions_axioms.mli.expected} %{dep:basic_functions_axioms.mli.output})))

(rule
 (target complex_vals.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:complex_vals.mli}))))

(rule
 (target complex_vals.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:complex_vals.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:complex_vals.mli.expected} %{dep:complex_vals.mli.output})))

(rule
 (target exceptions.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:exceptions.mli}))))

(rule
 (target exceptions.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:exceptions.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:exceptions.mli.expected} %{dep:exceptions.mli.output})))

(rule
 (target modules.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:modules.mli}))))

(rule
 (target modules.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:modules.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:modules.mli.expected} %{dep:modules.mli.output})))

(rule
 (target more_types.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:more_types.mli}))))

(rule
 (target more_types.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:more_types.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:more_types.mli.expected} %{dep:more_types.mli.output})))

(rule
 (target type_decl.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:type_decl.mli}))))

(rule
 (target type_decl.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:type_decl.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:type_decl.mli.expected} %{dep:type_decl.mli.output})))

(rule
 (target vals.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:vals.mli}))))

(rule
 (target vals.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:vals.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:vals.mli.expected} %{dep:vals.mli.output})))
