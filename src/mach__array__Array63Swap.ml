let swap (a: ('a array)) (i: int) (j: int) : unit =
  let v = a.(i) in begin let o = a.(j) in a.(i) <- o; a.(j) <- v end

