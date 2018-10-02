exception OutOfBounds

let defensive_get (a: ('a array)) (i: int) : 'a =
  begin
    if (i < 0) || (i >= (Array.length a)) then begin raise OutOfBounds end;
    a.(i)
  end

let defensive_set (a: ('a array)) (i: int) (v: 'a) : unit =
  begin
    if (i < 0) || (i >= (Array.length a)) then begin raise OutOfBounds end;
    a.(i) <- v
  end

let init (n: int) (f: (int) -> 'a) : ('a array) =
  let a = Array.make n (f 0) in
  begin
    let o = n - 1 in let o1 = 1 in for i = o1 to o do a.(i) <- (f i) done; a
  end

