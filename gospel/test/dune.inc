
(rule
 (target a.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:a.mli}))))

(rule
 (target a.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:a.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:a.mli.expected} %{dep:a.mli.output})))

(rule
 (target a1.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:a1.mli}))))

(rule
 (target a1.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:a1.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:a1.mli.expected} %{dep:a1.mli.output})))

(rule
 (target a2.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:a2.mli}))))

(rule
 (target a2.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:a2.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:a2.mli.expected} %{dep:a2.mli.output})))

(rule
 (target a3.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:a3.mli}))))

(rule
 (target a3.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:a3.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:a3.mli.expected} %{dep:a3.mli.output})))

(rule
 (target b.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:b.mli}))))

(rule
 (target b.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:b.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:b.mli.expected} %{dep:b.mli.output})))

(rule
 (target c.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:c.mli}))))

(rule
 (target c.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:c.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:c.mli.expected} %{dep:c.mli.output})))

(rule
 (target test.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:test.mli}))))

(rule
 (target test.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:test.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:test.mli.expected} %{dep:test.mli.output})))

(rule
 (target test1.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:test1.mli}))))

(rule
 (target test1.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:test1.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:test1.mli.expected} %{dep:test1.mli.output})))

(rule
 (target test2.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:test2.mli}))))

(rule
 (target test2.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:test2.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:test2.mli.expected} %{dep:test2.mli.output})))
