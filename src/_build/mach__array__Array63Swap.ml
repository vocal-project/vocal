let swap (a: ('a array)) (i: int) (j: int) : unit =
  let v = Array.get a i in
  begin let o = Array.get a j in Array.set a i o; Array.set a j v end

