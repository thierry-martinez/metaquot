let () =
  match [%expr 1 + 2] with
  | { pexp_desc = Pexp_apply (
      { pexp_desc = Pexp_ident { txt = Lident "+" }},
      [(Nolabel, { pexp_desc = Pexp_constant (Pconst_integer ("1", None))});
        (Nolabel, { pexp_desc = Pexp_constant (Pconst_integer ("2", None)) })])}
    -> ()
  | _ -> assert false

let () =
  match [%pat? ("a", None)] with
  | { ppat_desc = Ppat_tuple [
      { ppat_desc = Ppat_constant (Pconst_string ("a", None))};
      { ppat_desc = Ppat_construct ({ txt = Lident "None" }, None)}]} ->
    ()
  | _ -> assert false
