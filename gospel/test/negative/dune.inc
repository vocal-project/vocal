
(rule
 (target t1.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t1.mli}))))

(rule
 (target t1.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t1.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t1.mli.expected} %{dep:t1.mli.output})))

(rule
 (target t10.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t10.mli}))))

(rule
 (target t10.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t10.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t10.mli.expected} %{dep:t10.mli.output})))

(rule
 (target t11.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t11.mli}))))

(rule
 (target t11.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t11.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t11.mli.expected} %{dep:t11.mli.output})))

(rule
 (target t12.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t12.mli}))))

(rule
 (target t12.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t12.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t12.mli.expected} %{dep:t12.mli.output})))

(rule
 (target t13.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t13.mli}))))

(rule
 (target t13.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t13.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t13.mli.expected} %{dep:t13.mli.output})))

(rule
 (target t14.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t14.mli}))))

(rule
 (target t14.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t14.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t14.mli.expected} %{dep:t14.mli.output})))

(rule
 (target t15.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t15.mli}))))

(rule
 (target t15.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t15.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t15.mli.expected} %{dep:t15.mli.output})))

(rule
 (target t16.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t16.mli}))))

(rule
 (target t16.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t16.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t16.mli.expected} %{dep:t16.mli.output})))

(rule
 (target t17.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t17.mli}))))

(rule
 (target t17.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t17.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t17.mli.expected} %{dep:t17.mli.output})))

(rule
 (target t18.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t18.mli}))))

(rule
 (target t18.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t18.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t18.mli.expected} %{dep:t18.mli.output})))

(rule
 (target t19.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t19.mli}))))

(rule
 (target t19.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t19.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t19.mli.expected} %{dep:t19.mli.output})))

(rule
 (target t2.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t2.mli}))))

(rule
 (target t2.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t2.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t2.mli.expected} %{dep:t2.mli.output})))

(rule
 (target t20.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t20.mli}))))

(rule
 (target t20.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t20.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t20.mli.expected} %{dep:t20.mli.output})))

(rule
 (target t21.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t21.mli}))))

(rule
 (target t21.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t21.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t21.mli.expected} %{dep:t21.mli.output})))

(rule
 (target t22.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t22.mli}))))

(rule
 (target t22.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t22.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t22.mli.expected} %{dep:t22.mli.output})))

(rule
 (target t23.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t23.mli}))))

(rule
 (target t23.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t23.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t23.mli.expected} %{dep:t23.mli.output})))

(rule
 (target t24.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t24.mli}))))

(rule
 (target t24.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t24.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t24.mli.expected} %{dep:t24.mli.output})))

(rule
 (target t25.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t25.mli}))))

(rule
 (target t25.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t25.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t25.mli.expected} %{dep:t25.mli.output})))

(rule
 (target t26.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t26.mli}))))

(rule
 (target t26.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t26.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t26.mli.expected} %{dep:t26.mli.output})))

(rule
 (target t27.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t27.mli}))))

(rule
 (target t27.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t27.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t27.mli.expected} %{dep:t27.mli.output})))

(rule
 (target t28.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t28.mli}))))

(rule
 (target t28.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t28.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t28.mli.expected} %{dep:t28.mli.output})))

(rule
 (target t29.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t29.mli}))))

(rule
 (target t29.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t29.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t29.mli.expected} %{dep:t29.mli.output})))

(rule
 (target t3.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t3.mli}))))

(rule
 (target t3.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t3.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t3.mli.expected} %{dep:t3.mli.output})))

(rule
 (target t30.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t30.mli}))))

(rule
 (target t30.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t30.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t30.mli.expected} %{dep:t30.mli.output})))

(rule
 (target t31.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t31.mli}))))

(rule
 (target t31.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t31.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t31.mli.expected} %{dep:t31.mli.output})))

(rule
 (target t32.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t32.mli}))))

(rule
 (target t32.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t32.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t32.mli.expected} %{dep:t32.mli.output})))

(rule
 (target t33.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t33.mli}))))

(rule
 (target t33.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t33.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t33.mli.expected} %{dep:t33.mli.output})))

(rule
 (target t34.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t34.mli}))))

(rule
 (target t34.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t34.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t34.mli.expected} %{dep:t34.mli.output})))

(rule
 (target t35.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t35.mli}))))

(rule
 (target t35.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t35.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t35.mli.expected} %{dep:t35.mli.output})))

(rule
 (target t36.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t36.mli}))))

(rule
 (target t36.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t36.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t36.mli.expected} %{dep:t36.mli.output})))

(rule
 (target t37.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t37.mli}))))

(rule
 (target t37.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t37.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t37.mli.expected} %{dep:t37.mli.output})))

(rule
 (target t38.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t38.mli}))))

(rule
 (target t38.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t38.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t38.mli.expected} %{dep:t38.mli.output})))

(rule
 (target t39.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t39.mli}))))

(rule
 (target t39.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t39.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t39.mli.expected} %{dep:t39.mli.output})))

(rule
 (target t4.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t4.mli}))))

(rule
 (target t4.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t4.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t4.mli.expected} %{dep:t4.mli.output})))

(rule
 (target t5.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t5.mli}))))

(rule
 (target t5.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t5.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t5.mli.expected} %{dep:t5.mli.output})))

(rule
 (target t6.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t6.mli}))))

(rule
 (target t6.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t6.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t6.mli.expected} %{dep:t6.mli.output})))

(rule
 (target t7.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t7.mli}))))

(rule
 (target t7.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t7.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t7.mli.expected} %{dep:t7.mli.output})))

(rule
 (target t8.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t8.mli}))))

(rule
 (target t8.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t8.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t8.mli.expected} %{dep:t8.mli.output})))

(rule
 (target t9.mli.pps)
 (action
   (with-stdout-to %{target}
     (run gospel_pps %{dep:t9.mli}))))

(rule
 (target t9.mli.output)
 (action
    (with-outputs-to %{target}
     (with-accepted-exit-codes
      (or :standard 125)
      (run gospel tc --print-intermediate %{dep:t9.mli.pps})))))

(rule
 (alias runtest)
 (action (diff %{dep:t9.mli.expected} %{dep:t9.mli.output})))
