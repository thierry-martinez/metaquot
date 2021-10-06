let () =
  match
    [%stri type t [@@subst let t : string = name]
      [@@for name := ["t1"; "t2"; "t3"]]]
  with
  | { pstr_desc = Pstr_type (Recursive, [
        { ptype_name = { txt = "t1"; _ }; _};
        { ptype_name = { txt = "t2"; _ }; _};
        { ptype_name = { txt = "t3"; _ }; _}]); _ } -> ()
  | _ -> assert false
