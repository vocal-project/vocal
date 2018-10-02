let rec nth : 'a . (Z.t) -> ('a list) -> ('a option) =
  fun n l ->
    begin match l with
    | [] -> None
    | x :: r ->
      if Z.equal n Z.zero then begin Some x end
      else
      begin
        nth (Z.sub n Z.one) r end
    end

