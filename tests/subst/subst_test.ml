let () =
  match [%stri module M = struct end][@subst let M : string = "Test"] with
  | { pstr_desc = Pstr_module {
        pmb_name = { txt = Some "Test"; _ };
        pmb_expr = { pmod_desc = Pmod_structure []; _ };
        pmb_attributes = [];
        _ }; _} -> ()
  | _ -> assert false
let () =
  match [%stri type t [@@subst let t : string = "test"]] with
  | { pstr_desc = Pstr_type (Recursive,
        [{ ptype_name = { txt = "test"; _ }; _}]); _} -> ()
  | _ -> assert false

let () =
  match [%expr match x with c -> .]
    [@subst let c : list = [
      Ppxlib.Ast_helper.Exp.case (Ppxlib.Ast_helper.Pat.any ())
        (Metapp.Exp.var "x")]] with
  | { pexp_desc = Pexp_match (
      { pexp_desc = Pexp_ident { txt = Lident "x"; _ }; _ }, [
          { pc_lhs = { ppat_desc = Ppat_any; _};
            pc_rhs = { pexp_desc = Pexp_ident { txt = Lident "x"; _ }; _}; _}]);
      _}
    -> ()
  | _ -> assert false
