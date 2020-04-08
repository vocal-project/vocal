type 'a t = {
  mutable first: 'a SinglyLL.cell;
  mutable last: 'a SinglyLL.cell;
  }

exception Empty

let create : type a. unit ->  (a t) =
  fun _ -> { first = SinglyLL.Nil ; last = SinglyLL.Nil  }

let is_empty : type a. (a t) ->  (bool) =
  fun q -> ((q.first) == (SinglyLL.Nil ))

let add : type a. a -> (a t) ->  unit =
  fun x q -> let c = (SinglyLL.Cons { content = x; next = (SinglyLL.Nil ) }) in
             if ((q.last) == (SinglyLL.Nil ))
             then begin q.first <- c; q.last <- c end
             else begin (SinglyLL.set_next (q.last) c); q.last <- c end

let take_opt : type a. (a t) ->  (a option) =
  fun q -> if is_empty q
           then None 
           else
             begin
               let x = (SinglyLL.get_content (q.first)) in
               if ((SinglyLL.get_next (q.first)) == (SinglyLL.Nil ))
               then
                 begin q.first <- SinglyLL.Nil ; q.last <- SinglyLL.Nil  end
               else q.first <- (SinglyLL.get_next (q.first));
               Some x end

let take : type a. (a t) ->  a =
  fun q -> match take_opt q with
    | None -> raise Empty
    | Some e -> e

let peek : type a. (a t) ->  a =
  fun q -> if is_empty q then raise Empty; (SinglyLL.get_content (q.first))

let peek_opt : type a. (a t) ->  (a option) =
  fun q -> if is_empty q
           then None 
           else Some (SinglyLL.get_content (q.first))

let clear : type a. (a t) ->  unit =
  fun q -> q.first <- SinglyLL.Nil ; q.last <- SinglyLL.Nil 

let transfer : type a. (a t) -> (a t) ->  unit =
  fun q1 q2 -> if not (is_empty q1)
               then begin
                 if is_empty q2
                 then
                   begin
                     begin q2.first <- q1.first; q2.last <- q1.last end;
                     begin q1.first <- SinglyLL.Nil ;
                     q1.last <- SinglyLL.Nil  end
                   end
                 else
                   begin
                     (SinglyLL.set_next (q2.last) (q1.first));
                     q2.last <- q1.last;
                     begin q1.first <- SinglyLL.Nil ;
                     q1.last <- SinglyLL.Nil  end
                   end end

