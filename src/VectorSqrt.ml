open Printf


(**
A data structure that implements a vector interface, 
with O(1) push, pop, and random access, and whose space
usage is (1+epsilon)*n + O(sqrt(n)), for storing n words,
where epsilon is a small constant (for array headers).
Thus, compared with traditional vectors, this structure 
improves space efficiency. Moreover, items are never copied,
so constant factors may be improved. This comes at the
cost of a little bit of arithmetic computations on each
random access in the array.

Arthur Charguéraud, 2016

*)

let min_capacity = 4 (* 1 *)

type 'a t =
  { mutable rest_size : int; (* size = rest_size + head_size;  0 <= size; 
                                if head_size = 0 then rest_size = 0 *)
    mutable head_size : int; (* 0 <= head_size <= head.length;  only zero if structure is empty. *)
    mutable head : 'a array;  (* head of length 2^k * min_capacity *)
    mutable spine : ('a array) Vector.t;  (* top element of spine is head *)
    dummy : 'a; }

let create dummy =
   let t = Array.make min_capacity dummy in
   let v = Vector.create [||] in
   Vector.push v t;
   { rest_size = 0;
     head_size = 0;
     head = t;
     spine = v; 
     dummy = dummy; }

(* computes whether n is of the form 2^2k, for k>=0 *)
let rec is_power_of_4 i =
   if i = 1 then true 
   else if (i mod 4) = 0 then is_power_of_4 (i lsr 2)
   else false

(* computes k such that 2^(2*k) <= i < 2^(2*(k+1)),
   assuming i >= 4. *)
let exp_power_of_4 i =
   assert (i >= 4);
   let x = ref 0 in
   let r = ref i in
   let r32 = !r lsr 32 in
   if r32 > 0 then (r := r32; x := !x+16);
   let r16 = !r lsr 16 in
   if r16 > 0 then (r := r16; x := !x+8);
   let r8 = !r lsr 8 in
   if r8 > 0 then (r := r8; x := !x+4);
   let v = !r in
   if v >= 64 then
      !x + 3 
   else if v >= 16 then
      !x + 2
   else if v >= 4 then
      !x + 1
   else
      !x

let is_empty s =
   assert (if s.head_size = 0 then s.rest_size = 0 else true);
   s.head_size = 0 

let size s =
   s.rest_size + s.head_size

let push s x =
   let head_capacity = Array.length s.head in
   if s.head_size = head_capacity then begin
     (* current chunk is full, so allocate a new one *)
     let i = (size s) / min_capacity in
     let new_capacity =  
       if i >= 4 && is_power_of_4 i then 2*head_capacity else head_capacity in
     let t = Array.make new_capacity s.dummy in
     s.rest_size <- s.rest_size + head_capacity;
     s.head_size <- 0;
     s.head <- t;
     Vector.push s.spine t;
   end;
   assert (s.head_size < Array.length s.head);
   let j = s.head_size in
   s.head_size <- j+1;
   s.head.(j) <- x

let pop s =
   assert (not (is_empty s));
   let j = s.head_size - 1 in
   assert (j >= 0);
   s.head_size <- j;
   let x = s.head.(j) in
   s.head.(j) <- s.dummy;
   if j = 0 && s.rest_size > 0 then begin
      (* current chunk is empty and needs to be removed *)
      ignore (Vector.pop s.spine); (* should return s.head *)
      s.head <- Vector.top s.spine;
      let head_capacity = Array.length s.head in
      s.rest_size <- s.rest_size - head_capacity;
      s.head_size <- head_capacity;
   end;
   x

let get s i =
   let d = i mod min_capacity in
   let i = i / min_capacity in
   if i < 4 then begin
      let t = Vector.get s.spine i in
      t.(d)
   end else begin
      (* i = 2^(2k) + r    thus  r = i - 1<<(2k)
         r = b*2^k + j     thus  b = r >> k   and   j = r - (b<<k)
         a = 3*2^k - 2,    is the index of the first chunk of size 2^k
       *)
      let k = exp_power_of_4 i in
      let a = (3 lsl k) - 2 in
      let r = i - (1 lsl (2*k)) in
      let b = (r lsr k) in
      let j = r - (b lsl k) in
      assert (1 <= k && k < 64);
      assert (0 <= a);
      assert (0 <= r && r < 3*(1 lsl (2*k)));
      assert (0 <= j && j < (1 lsl k));
      let t = Vector.get s.spine (a+b) in
      if (Array.length t <> (1 lsl k)*min_capacity) then begin
         printf "i=%d k=%d a=%d r=%d b=%d j=%d len=%d\n" i k a r b j (Array.length t);
         assert false;
      end;
      assert (Array.length t = (1 lsl k)*min_capacity);
      t.(j * min_capacity + d)
   end



(*---------------------------------------------------*)


let debug = false
let ndebug = 2800000

(*
let debug = true
let ndebug = 56
*)

(* Bruteforce testing for exp_power_of_4
   let _ = 
      for i = 4 to 1000025 do
         let k = exp_power_of_4 i in
         assert ((1 lsl (2*k)) <= i && i < (1 lsl (2*(k+1))));
      done;
      exit 0
*)

(* Bruteforce testing for is_power_of_4
   let _ = 
      for i = 4 to 1000025 do
         let b = is_power_of_4 i in
         let k = exp_power_of_4 i in
         if b && not ((1 lsl (2*k)) = i) then begin
            printf "bug on i=%d k=%d\n" i k;
            assert false;
         end
      done;
      exit 0
*)


let print s =
   if debug then begin
      printf "==================\nrest_size = %d\nhead_size = %d\n" s.rest_size s.head_size;
      Vector.iteri s.spine (fun i t -> 
         printf "%d -> [%d] " i (Array.length t);
         (* Array.iter (fun x -> printf "%d " x) t;  *)
         printf "\n");
     printf "\n";
  end

let _ = 
   let s = create (-1) in
   print s;
   for i = 0 to pred ndebug do
      push s i;
      if debug then printf "---- after push %d ---\n" i;
      print s;
   done;
   for i = 0 to pred ndebug do
      let x = get s i in
      if debug then printf "---- perfom get %d obtain %d ---\n" i x;
      assert (x = i);
   done;
   for i = ndebug-1 downto 0 do
      let x = pop s in
      assert (x = i);
      let e = (is_empty s) in
      assert (if e then i = 0 else i > 0);
      if debug then printf "---- after pop %d ---\n" i;
      print s;
   done
      
