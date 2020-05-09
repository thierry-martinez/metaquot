let () =
  match [%expr 1 + 2] with
  | { pexp_desc = Pexp_apply (
      { pexp_desc = Pexp_ident { txt = Lident "+"; _ }; _ },
      [(Nolabel, { pexp_desc = Pexp_constant (Pconst_integer ("1", None)); _ });
        (Nolabel,
          { pexp_desc = Pexp_constant (Pconst_integer ("2", None)); _ })]); _ }
    -> ()
  | _ -> assert false

let () =
  match [%pat? ("a", None)] with
  | { ppat_desc = Ppat_tuple [
      { ppat_desc = Ppat_constant constant; _ };
      { ppat_desc =
        Ppat_construct ({ txt = Lident "None"; _ }, None); _ }]; _ } ->
    begin match Metapp.destruct_string_constant constant with
    | Some { s = "a"; delim = None; _ } -> ()
    | _ -> assert false
    end
  | _ -> assert false

let () =
  match [%type: bool * char] with
  | [%type: int] -> assert false
  | [%type: bool * char] -> ()
  | _ -> assert false
