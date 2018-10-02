let is_none (o: 'a option) : bool =
  begin match o with
  | None -> true
  | Some _ -> false
  end

