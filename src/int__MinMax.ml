let min (x: Z.t) (y: Z.t) : Z.t =
  if Z.leq x y then begin x end else begin y end

let max (x: Z.t) (y: Z.t) : Z.t =
  if Z.leq x y then begin y end else begin x end

