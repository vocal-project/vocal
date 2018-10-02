type 'a t = ('a, unit) Hashtbl.t

let create ?random:(random: (bool) option) (n: int) : 'a t =
  Hashtbl.create ?random:random n

let add (s: 'a t) (k: 'a) : unit = Hashtbl.replace s k ()

let mem (s: 'a t) (k: 'a) : bool = Hashtbl.mem s k

let remove (s: 'a t) (k: 'a) : unit = Hashtbl.remove s k

let length (s: 'a t) : int = Hashtbl.length s



