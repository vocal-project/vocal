(*@ Specification file, implicitly comment out all lines *)

open math with implicit instances

instance neg (x:int) : int
instance add (x1 x2:int)  : int
instance sub (x1 x2:int)  : int
instance mul (x1 x2:int) : int
instance div (x1 x2:int) : int
instance abs (x:int) : int
instance min (x1 x2:int) : int
instance max (x1 x2:int) : int
instance lt (x1 x2:int) : prop
instance gt (x1 x2:int) : prop
instance le (x1 x2:int) : prop
instance ge (x1 x2:int) : prop

value max_int : int
value min_int : int

