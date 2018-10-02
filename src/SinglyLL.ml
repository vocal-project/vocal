type 'a cell = Nil | Cons of { mutable content: 'a; mutable next: 'a cell; }

let get_content = function
  | Nil -> invalid_arg "get_content"
  | Cons { content } -> content

let set_content l v = match l with
  | Nil -> invalid_arg "set_content"
  | Cons c -> c.content <- v

let get_next = function
  | Nil -> invalid_arg "get_next"
  | Cons { next } -> next

let set_next l1 l2 = match l1 with
  | Nil -> invalid_arg "set_next"
  | Cons c -> c.next <- l2

let rec iter f l = match l with
  | Nil -> ()
  | Cons { content; next } -> f content; iter f next
