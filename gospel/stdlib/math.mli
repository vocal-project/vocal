(*@ Specification file, implicitly comment out all lines *)

(* declaration of overloaded symbols is implicit,
   except when we wish to attach a notation to them *)

(* arithmetic *)

overloaded neg
overloaded sub
overloaded add
overloaded mul
overloaded div
overloaded modulo
overloaded lt
overloaded gt
overloaded le
overloaded ge

prefix notation (-_) := neg
infix notation (+) := add
infix notation (-) := sub
infix notation ( * ) := mul
infix notation (/) := mul
infix notation (mod) := modulo
infix notation (<) := lt
infix notation (>) := gt
infix notation (<=) := le (* only parsing *)
infix notation (>=) := ge (* only parsing *)
infix notation (≤) := le
infix notation (≥) := ge


derived instance (-) (x y:'a) : 'a :=
  x + (-_ y)
derived instance (>) (x y:'a) : 'a :=
  (<) y x
derived instance (>=) (x y:'a) : 'a :=
  (<=) y x
derived instance (<=) (x y:'a) : 'a :=
  y < x || x = y

(* containers *)

overloaded empty
overloaded mem
overloaded union
overloaded inter
overloaded diff
overloaded incl
overloaded read
overloaded update

notation "∅" := empty
infix notation "∈" := mem
infix notation "x ∉ s" := (~ (mem x s))
infix notation "∪" := union
infix notation "∩" := inter
infix notation "∖" := diff
infix notation "⊆" := incl

notation "`{ x }" := (single x)
notation "m [ k ] " := (read m k)
notation "m [ k := v] " := (update m k v)

overloaded dom