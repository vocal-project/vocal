type 'a t = {
  mutable len: int;
  mutable first: 'a SinglyLL.cell;
  mutable last: 'a SinglyLL.cell;
  }

let create (us: unit) : 'a t =
  { len = 0; first = SinglyLL.Nil; last = SinglyLL.Nil }

let length (q: 'a t) : int = q.len

let is_empty (q: 'a t) : bool = (q.len) = (0)

let clear (q: 'a t) : unit =
  begin
    let o = 0 in q.len <- o; let o = SinglyLL.Nil in q.first <- o;
    let o = SinglyLL.Nil in q.last <- o
  end

let add (x: 'a) (q: 'a t) : unit =
  let c = SinglyLL.Cons { content = x; next = SinglyLL.Nil } in
  if (q.len) = (0) then begin
    begin q.first <- c; q.last <- c; let o = 1 in q.len <- o end end
  else
  begin
    begin
      SinglyLL.set_next (q.last) c; q.last <- c; let o = (q.len) + 1 in
      q.len <- o
    end end

exception Empty

let peek (q: 'a t) : 'a =
  begin
    if (q.len) = (0) then begin raise Empty end;
    SinglyLL.get_content (q.first)
  end

let take (q: 'a t) : 'a =
  begin
    if (q.len) = (0) then begin raise Empty end;
    let x = SinglyLL.get_content (q.first) in
    begin
      if (q.len) = (1) then begin
        begin
          let o = SinglyLL.Nil in q.first <- o; let o = SinglyLL.Nil in
          q.last <- o; let o = 0 in q.len <- o
        end end
      else
      begin
        let o = (q.len) - 1 in
        let o1 = SinglyLL.get_next (q.first) in
        begin q.first <- o1; q.len <- o end end;
      x
    end
  end

let copy (q: 'a t) : 'a t =
  if (q.len) = (0) then begin create () end
  else
  begin
    let newq = create () in
    let rec visit (c: 'a SinglyLL.cell) : unit =
      if not (c == SinglyLL.Nil)
      then begin
        begin
          add (SinglyLL.get_content c) newq; visit (SinglyLL.get_next c)
        end end in
    begin visit (q.first); newq end end

let transfer (q1: 'a t) (q2: 'a t) : unit =
  if not (is_empty q1)
  then begin
    if is_empty q2 then begin
      begin
        let o = 0 in let o1 = q1.len in begin q2.len <- o1; q1.len <- o end;
        let o = q1.last in
        let o1 = q1.first in begin q2.first <- o1; q2.last <- o end;
        let o = SinglyLL.Nil in
        let o1 = SinglyLL.Nil in begin q1.first <- o1; q1.last <- o end
      end end
    else
    begin
      let len = (q2.len) + (q1.len) in
      begin
        let o = 0 in begin q2.len <- len; q1.len <- o end;
        SinglyLL.set_next (q2.last) (q1.first); let o = q1.last in
        q2.last <- o; let o = SinglyLL.Nil in
        let o1 = SinglyLL.Nil in begin q1.first <- o1; q1.last <- o end
      end end end

let fold (f: 'b -> ('a -> 'b)) (acc: 'b) (q: 'a t) : 'b =
  let rec fold (acc1: 'b) (c: 'a SinglyLL.cell) : 'b =
    if c == SinglyLL.Nil then begin acc1 end
    else
    begin
      fold ((f acc1) (SinglyLL.get_content c)) (SinglyLL.get_next c) end in
  fold acc (q.first)


let push x q = add x q

let pop q = take q

let top q =
  peek q




let iter f q =
  SinglyLL.iter f q.first
