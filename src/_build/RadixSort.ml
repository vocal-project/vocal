
(** Radix sort *)

module Strings = struct

  (** sort an array of strings [a] according to the first [w]
      characters of each string (Algorithms, 4th edition, p. 707) *)
  let index s i =
    if i >= String.length s then invalid_arg "lsd";
    Char.code s.[i]

  let lsd a w =
    let n = Array.length a in
    let aux = Array.make n "" in
    let r = 256 in
    for d = w-1 downto 0 do
      (* compute frequency counts *)
      let count = Array.make (r+1) 0 in
      Array.iter (fun s ->
        let i = index s d + 1 in count.(i) <- count.(i) + 1) a;
      (* transform sums to indices *)
      for i = 0 to r-1 do count.(i+1) <- count.(i+1) + count.(i) done;
      (* distribute *)
      Array.iter (fun s ->
        let i = index s d in
        aux.(count.(i)) <- s; count.(i) <- count.(i) + 1) a;
      (* copy back *)
      Array.iteri (fun i s -> a.(i) <- s) aux
    done

  (** sort an array of strings (Algorithms, 4th edition, p. 712) *)
  let index s i =
    if i >= String.length s then 0 else 1 + Char.code s.[i]

  let rec sort aux a lo hi d =
    (* FIXME use insertion sort for small segments *)
    if lo < hi-1 then begin
      let r = 256 in
      let count = Array.make (r+2) 0 in
      for i = lo to hi-1 do
        let j = index a.(i) d + 1 in count.(j) <- count.(j) + 1
      done;
      for i = 0 to r do count.(i+1) <- count.(i+1) + count.(i) done;
      for i = lo to hi-1 do
        let j = index a.(i) d in
        aux.(count.(j)) <- a.(i); count.(j) <- count.(j) + 1
      done;
      Array.blit aux 0 a lo (hi - lo);
      for i = 0 to r do sort aux a (lo+count.(i)) (lo+count.(i+1)) (d+1) done
    end

  let msd a =
    let n = Array.length a in
    let aux = Array.make n "" in
    sort aux a 0 n 0

end

module Ints = struct

  (** sort integers *)
  let sort a =
    let n = Array.length a in
    let aux = Array.make n 0 in
    let r = 256 in
    let count = Array.make (r+1) 0 in
    for sh = 0 to Sys.word_size lsr 3 - 2 do
      let sh = 8 * sh in
      Array.fill count 0 (r+1) 0;
      Array.iter (fun x ->
        let i = ((x lsr sh) land 255) + 1 in count.(i) <- count.(i) + 1) a;
      for i = 0 to r-1 do count.(i+1) <- count.(i+1) + count.(i) done;
      Array.iter (fun x ->
        let i = (x lsr sh) land 255 in
        aux.(count.(i)) <- x; count.(i) <- count.(i) + 1) a;
      Array.iteri (fun i x -> a.(i) <- x) aux;
    done;
    (* make a last pass where the sign bit is interpreted *)
    let sh = Sys.word_size - 8 in
    (* compute frequency counts *)
    Array.fill count 0 (r+1) 0;
    Array.iter (fun x ->
      let i = (x asr sh + 64) + 1 in count.(i) <- count.(i) + 1) a;
    for i = 0 to r-1 do count.(i+1) <- count.(i+1) + count.(i) done;
    Array.iter (fun x ->
      let i = x asr sh + 64 in
      aux.(count.(i)) <- x; count.(i) <- count.(i) + 1) a;
    (* copy back *)
    Array.iteri (fun i x -> a.(i) <- x) aux

end


