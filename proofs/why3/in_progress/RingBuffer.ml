type 'a buffer = {
  mutable first: int;
  mutable len: int;
  data: ('a array);
  }

let create : type a. (int) -> a ->  (a buffer) =
  fun n dummy -> { first = 0; len = 0; data = Array.make n dummy }

let length : type a. (a buffer) ->  (int) = fun b -> b.len

let clear : type a. (a buffer) ->  unit = fun b -> b.len <- 0

let push : type a. (a buffer) -> a ->  unit =
  fun b x -> let n = Array.length b.data in
             let i =
               if b.first >= n - b.len
               then b.first + (b.len - n)
               else b.first + b.len in
             (b.data).(i) <- x; b.len <- b.len + 1

let peek : type a. (a buffer) ->  a = fun b -> (b.data).(b.first)

let pop : type a. (a buffer) ->  a =
  fun b -> let r = (b.data).(b.first) in
           b.len <- b.len - 1;
           let n = Array.length b.data in
           b.first <- b.first + 1; if b.first = n then b.first <- 0; r
