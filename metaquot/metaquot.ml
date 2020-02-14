[%%metapackage "metapp.utils"]

let ast_helper = Longident.Lident "Ast_helper"

let longident = Longident.Lident "Longident"

let parsetree = Longident.Lident "Parsetree"

let ast_helper_const = Longident.Ldot (ast_helper, "Const")

let ast_helper_attr = Longident.Ldot (ast_helper, "Attr")

let ast_helper_typ = Longident.Ldot (ast_helper, "Typ")

let ast_helper_pat = Longident.Ldot (ast_helper, "Pat")

let ast_helper_exp = Longident.Ldot (ast_helper, "Exp")

let ast_helper_val = Longident.Ldot (ast_helper, "Val")

let ast_helper_type = Longident.Ldot (ast_helper, "Type")

let ast_helper_te = Longident.Ldot (ast_helper, "Te")

let ast_helper_mty = Longident.Ldot (ast_helper, "Mty")

let ast_helper_mod = Longident.Ldot (ast_helper, "Mod")

let ast_helper_sig = Longident.Ldot (ast_helper, "Sig")

let ast_helper_str = Longident.Ldot (ast_helper, "Str")

let ast_helper_md = Longident.Ldot (ast_helper, "Md")

let ast_helper_ms = Longident.Ldot (ast_helper, "Ms")

let ast_helper_mtd = Longident.Ldot (ast_helper, "Mtd")

let ast_helper_mb = Longident.Ldot (ast_helper, "Mb")

let ast_helper_opn = Longident.Ldot (ast_helper, "Opn")

let ast_helper_incl = Longident.Ldot (ast_helper, "Incl")

let ast_helper_vb = Longident.Ldot (ast_helper, "Vb")

let ast_helper_cty = Longident.Ldot (ast_helper, "Cty")

let ast_helper_ctf = Longident.Ldot (ast_helper, "Ctf")

let ast_helper_cl = Longident.Ldot (ast_helper, "Cl")

let ast_helper_cf = Longident.Ldot (ast_helper, "Cf")

let ast_helper_ci = Longident.Ldot (ast_helper, "Ci")

let ast_helper_csig = Longident.Ldot (ast_helper, "Csig")

let ast_helper_cstr = Longident.Ldot (ast_helper, "Cstr")

let ast_helper_rf = Longident.Ldot (ast_helper, "Rf")

let ast_helper_of = Longident.Ldot (ast_helper, "Of")

let quote_unit = Metapp_utils.unit

let quote_string = Metapp_utils.expression_of_string

let quote_char = Metapp_utils.expression_of_char

let quote_default_loc = Metapp_utils.expression_of_default_loc

let quote_loc = Metapp_utils.expression_of_loc

let rec quote_longident (ident : Longident.t) : Parsetree.expression =
  match ident with
  | Ldot (m, v) ->
      Metapp_utils.construct (Ldot (longident, "Ldot"))
        [quote_longident m; quote_string v]
  | Lident ident ->
      Metapp_utils.construct (Ldot (longident, "Lident")) [quote_string ident]
  | Lapply (f, x) ->
      Metapp_utils.construct (Ldot (longident, "Lapply"))
        [quote_longident f; quote_longident x]

let quote_bool = Metapp_utils.bool

let quote_option (quote_value : 'a -> Parsetree.expression) (option : 'a option)
    : Parsetree.expression =
  match option with
  | None -> Metapp_utils.construct (Lident "None") []
  | Some v -> Metapp_utils.construct (Lident "Some") [quote_value v]

let quote_constant (constant : Parsetree.constant) : Parsetree.expression =
  match constant with
  | Pconst_integer (value, suffix) ->
      let suffix =
        Stdcompat.Option.to_list (Stdcompat.Option.map (fun suffix ->
          "suffix", quote_char suffix) suffix) in
      Metapp_utils.apply_labels
        (Metapp_utils.ident (Ldot (ast_helper_const, "integer")))
        suffix [quote_string value]
  | Pconst_char c ->
      Metapp_utils.apply
        (Metapp_utils.ident (Ldot (ast_helper_const, "char"))) [quote_char c]
  | Pconst_string (s, delim) ->
      Metapp_utils.apply_labels
        (Metapp_utils.ident (Ldot (ast_helper_const, "string")))
        (Stdcompat.Option.to_list (
          (Stdcompat.Option.map
             (fun delim -> ("quotation_delimiter", quote_string delim))
             delim))) [quote_string s]
  | Pconst_float (value, suffix) ->
      Metapp_utils.apply (Metapp_utils.ident (Ldot (ast_helper_const, "float")))
        [quote_string value; quote_option quote_char suffix]

let asttypes = Longident.Lident "Asttypes"

let quote_rec_flag (flag : Asttypes.rec_flag) : Parsetree.expression =
  match flag with
  | Nonrecursive -> Metapp_utils.construct (Ldot (asttypes, "Nonrecursive")) []
  | Recursive -> Metapp_utils.construct (Ldot (asttypes, "Recursive")) []

let quote_direction_flag (flag : Asttypes.direction_flag)
    : Parsetree.expression =
  match flag with
  | Upto -> Metapp_utils.construct (Ldot (asttypes, "Upto")) []
  | Downto -> Metapp_utils.construct (Ldot (asttypes, "Downto")) []

let quote_private_flag (flag : Asttypes.private_flag) : Parsetree.expression =
  match flag with
  | Private -> Metapp_utils.construct (Ldot (asttypes, "Private")) []
  | Public -> Metapp_utils.construct (Ldot (asttypes, "Public")) []

let quote_mutable_flag (flag : Asttypes.mutable_flag) : Parsetree.expression =
  match flag with
  | Immutable -> Metapp_utils.construct (Ldot (asttypes, "Immutable")) []
  | Mutable -> Metapp_utils.construct (Ldot (asttypes, "Mutable")) []

let quote_virtual_flag (flag : Asttypes.virtual_flag) : Parsetree.expression =
  match flag with
  | Virtual -> Metapp_utils.construct (Ldot (asttypes, "Virtual")) []
  | Concrete -> Metapp_utils.construct (Ldot (asttypes, "Concrete")) []

let quote_closed_flag (flag : Asttypes.closed_flag) : Parsetree.expression =
  match flag with
  | Closed -> Metapp_utils.construct (Ldot (asttypes, "Closed")) []
  | Open -> Metapp_utils.construct (Ldot (asttypes, "Open")) []

let quote_override_flag (flag : Asttypes.override_flag) : Parsetree.expression =
  match flag with
  | Override -> Metapp_utils.construct (Ldot (asttypes, "Override")) []
  | Fresh -> Metapp_utils.construct (Ldot (asttypes, "Fresh")) []

let quote_arg_label (arg_label : Asttypes.arg_label) : Parsetree.expression =
  match arg_label with
  | Nolabel -> Metapp_utils.construct (Ldot (asttypes, "Nolabel")) []
  | Labelled label ->
      Metapp_utils.construct (Ldot (asttypes, "Labelled")) [quote_string label]
  | Optional label ->
      Metapp_utils.construct (Ldot (asttypes, "Optional")) [quote_string label]

let quote_variance (flag : Asttypes.variance) : Parsetree.expression =
  match flag with
  | Covariant -> Metapp_utils.construct (Ldot (asttypes, "Covariant")) []
  | Contravariant ->
      Metapp_utils.construct (Ldot (asttypes, "Contravariant")) []
  | Invariant -> Metapp_utils.construct (Ldot (asttypes, "Invariant")) []

let rec quote_list (quote_value : 'a -> Parsetree.expression) (list : 'a list)
    : Parsetree.expression =
  match list with
  | [] -> Metapp_utils.construct (Lident "[]") []
  | hd :: tl ->
      Metapp_utils.construct (Lident "::")
        [quote_value hd; quote_list quote_value tl]

let quote_pair (quote_fst : 'a -> Parsetree.expression)
    (quote_snd : 'b -> Parsetree.expression) ((a, b) : 'a * 'b)
    : Parsetree.expression =
  Ast_helper.Exp.tuple [quote_fst a; quote_snd b]

let quote_str = quote_loc quote_string

let quote_lid = quote_loc quote_longident

let quote_label = quote_string

let quote_stropt =
  [%meta if Sys.ocaml_version < "4.10.0" then
    [%e quote_str]
  else
    [%e quote_loc (quote_option quote_string)]]

let quote_str_405 =
  [%meta if Sys.ocaml_version < "4.05.0" then
    [%e quote_string]
  else
    [%e quote_str]]

[%%metadef
let ocaml_at_least version =
  Metapp_utils.bool (Sys.ocaml_version >= version)

let ocaml_before version =
  Metapp_utils.bool (Sys.ocaml_version < version)]

[%%meta
let open_pattern, apply_open =
  if Sys.ocaml_version < "4.08.0" then
    ([%p? (override_flag, lid, body)],
      (fun _ quote_body ->
        [%e apply "open_"
          [quote_override_flag override_flag; quote_lid lid;
            [%meta quote_body] body]]))
  else
    ([%p? (od, body)],
      (fun quote_od quote_body ->
        [%e apply "open_" [[%meta quote_od] od; [%meta quote_body] body]])) in
let functor_pattern, apply_functor =
  if Sys.ocaml_version < "4.10.0" then
    ([%p? (x, s, body)],
      (fun quote_body ->
        [%e apply "functor_"
          [quote_str x; quote_option quote_module_type s;
            [%meta quote_body] body]]))
  else
    ([%p? (f, body)],
      (fun quote_body ->
        [%e apply "functor_"
          [quote_functor_parameter f; [%meta quote_body] body]])) in
let quote_exception =
  [%e apply "exception_" [[%meta if Sys.ocaml_version >= "4.08.0" then
    [%e quote_type_exception exc]
  else
    [%e quote_extension_constructor exc]]]] in
Metapp_utils.filter.structure_item Metapp_utils.filter [%stri
let rec quote_core_type (ty : Parsetree.core_type) : Parsetree.expression =
  Ast_helper.with_default_loc ty.ptyp_loc @@ fun () ->
  let apply f args =
    Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_typ, f)))
      (label_arg_of_attributes ty.ptyp_attributes) args in
  match ty.ptyp_desc with
  | Ptyp_any ->
      apply "any" [quote_unit ()]
  | Ptyp_var x ->
      apply "var" [quote_string x]
  | Ptyp_arrow  (label, arg, body) ->
      apply "arrow"
        [quote_arg_label label; quote_core_type arg; quote_core_type body]
  | Ptyp_tuple args ->
      apply "tuple" [quote_list quote_core_type args]
  | Ptyp_constr (constr, args) ->
      apply "constr" [quote_lid constr; quote_list quote_core_type args]
  | Ptyp_object (fields, closed_flag) ->
      let[@when [%meta ocaml_before "4.06.0"]]
          quote_object_field (str, attr, ty) =
        Ast_helper.Exp.tuple
          [quote_str_405 str; quote_attributes attr; quote_core_type ty] in
      apply "object_"
        [quote_list quote_object_field fields; quote_closed_flag closed_flag]
  | Ptyp_class (class_name, args) ->
      apply "class_" [quote_lid class_name; quote_list quote_core_type args]
  | Ptyp_alias (ty, x) ->
      apply "alias" [quote_core_type ty; quote_string x]
  | Ptyp_variant (fields, closed_flag, labels) ->
      apply "variant"
        [quote_list quote_row_field fields; quote_closed_flag closed_flag;
          quote_option (quote_list quote_string) labels]
  | Ptyp_poly (vars, ty) ->
      apply "poly" [quote_list quote_str_405 vars; quote_core_type ty]
  | Ptyp_package (name, types) ->
      apply "poly"
        [quote_lid name;
          quote_list (quote_pair quote_lid quote_core_type) types]
  | Ptyp_extension ({ txt = "t"; _ }, payload) ->
      Metapp_utils.expression_of_payload payload
  | Ptyp_extension extension ->
      apply "extension" [quote_extension extension]
and quote_row_field (field : Parsetree.row_field) : Parsetree.expression =
  [%meta if Sys.ocaml_version < "4.08.0" then
    [%e match field with
    | Rtag (label, attrs, cstr, ty) ->
        Metapp_utils.apply (Metapp_utils.ident (Ldot (ast_helper_rf, "tag")))
          [(quote_loc [@when [%meta ocaml_at_least "4.06.0"]])
            quote_label label;
            quote_attributes attrs; quote_bool cstr;
            quote_list quote_core_type ty]
    | Rinherit ty ->
        Metapp_utils.apply
          (Metapp_utils.ident (Ldot (ast_helper_rf, "inherit_")))
          [quote_core_type ty] ]
  else
    [%e Ast_helper.with_default_loc field.prf_loc @@ fun () ->
    match field.prf_desc with
    | Rtag (label, cstr, ty) ->
        Metapp_utils.apply_labels
          (Metapp_utils.ident (Ldot (ast_helper_rf, "tag")))
          (label_arg_of_attributes field.prf_attributes)
          [quote_loc quote_label label; quote_bool cstr;
           quote_list quote_core_type ty]
    | Rinherit ty ->
        let result =
          Metapp_utils.apply
            (Metapp_utils.ident (Ldot (ast_helper_rf, "inherit_")))
            [quote_core_type ty] in
        match field.prf_attributes with
        | [] -> result
        | attributes ->
            Metapp_utils.apply_labels
              (Metapp_utils.ident (Ldot (ast_helper_rf, "mk")))
              (label_arg_of_attributes field.prf_attributes)
              [Ast_helper.Exp.field result
                (Metapp_utils.loc (Longident.Ldot (parsetree, "prf_desc")))]]]
and[@when [%meta ocaml_at_least "4.06.0"]]
    quote_object_field (field : Parsetree.object_field) : Parsetree.expression =
  [%meta if Sys.ocaml_version < "4.08.0" then
    [%e match field with
    | Otag (label, attrs, ty) ->
        Metapp_utils.apply (Metapp_utils.ident (Ldot (ast_helper_of, "tag")))
          [quote_loc quote_label label; quote_attributes attrs;
            quote_core_type ty]
    | Oinherit ty ->
        Metapp_utils.apply
          (Metapp_utils.ident (Ldot (ast_helper_of, "inherit_")))
          [quote_core_type ty]]
  else
    [%e Ast_helper.with_default_loc field.pof_loc @@ fun () ->
    match field.pof_desc with
    | Otag (label, ty) ->
        Metapp_utils.apply_labels
          (Metapp_utils.ident (Ldot (ast_helper_of, "tag")))
          (label_arg_of_attributes field.pof_attributes)
          [quote_loc quote_label label; quote_core_type ty]
    | Oinherit ty ->
        let result =
          Metapp_utils.apply
            (Metapp_utils.ident (Ldot (ast_helper_of, "inherit_")))
            [quote_core_type ty] in
        match field.pof_attributes with
        | [] -> result
        | attributes ->
            Metapp_utils.apply_labels
              (Metapp_utils.ident (Ldot (ast_helper_of, "mk")))
              (label_arg_of_attributes field.pof_attributes)
              [Ast_helper.Exp.field result
                (Metapp_utils.loc (Longident.Ldot (parsetree, "pof_desc")))]]]
and quote_pattern (pat : Parsetree.pattern) : Parsetree.expression =
  Ast_helper.with_default_loc pat.ppat_loc @@ fun () ->
  let apply f args =
    Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_pat, f)))
      (label_arg_of_attributes pat.ppat_attributes) args in
  match pat.ppat_desc with
  | Ppat_any ->
      apply "any" [quote_unit ()]
  | Ppat_var x ->
      apply "var" [quote_str x]
  | Ppat_alias (pat, x) ->
      apply "alias" [quote_pattern pat; quote_str x]
  | Ppat_constant constant ->
      apply "constant" [quote_constant constant]
  | Ppat_interval (lo, hi) ->
      apply "interval" [quote_constant lo; quote_constant hi]
  | Ppat_tuple args ->
      apply "tuple" [quote_list quote_pattern args]
  | Ppat_construct (lid, arg) ->
      apply "construct" [quote_lid lid; quote_option quote_pattern arg]
  | Ppat_variant (label, arg) ->
      apply "variant" [quote_label label; quote_option quote_pattern arg]
  | Ppat_record (fields, closed_flag) ->
      apply "record"
        [quote_list (quote_pair quote_lid quote_pattern) fields;
          quote_closed_flag closed_flag]
  | Ppat_array args ->
      apply "array" [quote_list quote_pattern args]
  | Ppat_or (left, right) ->
      apply "or_" [quote_pattern left; quote_pattern right]
  | Ppat_constraint (pat, ty) ->
      apply "constraint_" [quote_pattern pat; quote_core_type ty]
  | Ppat_type ty ->
      apply "type_" [quote_lid ty]
  | Ppat_lazy pat ->
      apply "lazy_" [quote_pattern pat]
  | Ppat_unpack str ->
      apply "unpack" [quote_stropt str]
  | Ppat_exception pat ->
      apply "exception_" [quote_pattern pat]
  | Ppat_extension ({ txt = "p"; _ }, payload) ->
      Metapp_utils.expression_of_payload payload
  | Ppat_extension e ->
      apply "extension" [quote_extension e]
  | Ppat_open (m, body)
      [@when [%meta ocaml_at_least "4.04.0"]] ->
      apply "open_" [quote_lid m; quote_pattern body]
and quote_expression (e : Parsetree.expression) : Parsetree.expression =
  Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  let apply f args =
    Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_exp, f)))
      (label_arg_of_attributes e.pexp_attributes) args in
  match e.pexp_desc with
  | Pexp_ident id ->
      apply "ident" [quote_lid id]
  | Pexp_constant constant ->
      apply "constant" [quote_constant constant]
  | Pexp_let (rec_flag, bindings, expr) ->
      apply "let_"
        [quote_rec_flag rec_flag; quote_list quote_value_binding bindings;
         quote_expression expr]
  | Pexp_function cases ->
      apply "function_" [quote_list quote_case cases]
  | Pexp_fun (label, default, pattern, expr) ->
      apply "fun_"
        [quote_arg_label label; quote_option quote_expression default;
          quote_pattern pattern; quote_expression expr]
  | Pexp_apply (f, args) ->
      apply "apply"
        [quote_expression f;
          quote_list (quote_pair quote_arg_label quote_expression) args]
  | Pexp_match (expr, cases) ->
      apply "match_" [quote_expression expr; quote_list quote_case cases]
  | Pexp_try (expr, cases) ->
      apply "try_" [quote_expression expr; quote_list quote_case cases]
  | Pexp_tuple args ->
      apply "tuple"
        [quote_list quote_expression args]
  | Pexp_construct (construct, arg) ->
      apply "construct"
        [quote_lid construct;
          quote_option quote_expression arg]
  | Pexp_variant (label, arg) ->
      apply "variant"
        [quote_string label; quote_option quote_expression arg]
  | Pexp_record (fields, base) ->
      apply "record"
        [quote_list (quote_pair (quote_lid) quote_expression)
           fields; quote_option quote_expression base]
  | Pexp_field (expr, field) ->
      apply "field"
        [quote_expression expr; quote_lid field]
  | Pexp_setfield (expr, field, value) ->
      apply "setfield"
        [quote_expression expr; quote_lid field;
          quote_expression value]
  | Pexp_array args ->
      apply "array"
        [quote_list quote_expression args]
  | Pexp_ifthenelse (condition, then_branch, else_branch) ->
      apply "ifthenelse"
        [quote_expression condition; quote_expression then_branch;
          quote_option quote_expression else_branch]
  | Pexp_sequence (a, b) ->
      apply "sequence"
        [quote_expression a; quote_expression b]
  | Pexp_while (condition, body) ->
      apply "while_"
        [quote_expression condition; quote_expression body]
  | Pexp_for (pattern, e1, e2, direction, body) ->
      apply "for_"
        [quote_pattern pattern; quote_expression e1; quote_expression e2;
          quote_direction_flag direction; quote_expression body]
  | Pexp_constraint (expr, ty) ->
      apply "constraint_"
        [quote_expression expr; quote_core_type ty]
  | Pexp_coerce (expr, ty0, ty) ->
      apply "coerce"
        [quote_expression expr; quote_option quote_core_type ty0;
          quote_core_type ty]
  | Pexp_send (obj, m) ->
      apply "send" [quote_expression obj; quote_str_405 m]
  | Pexp_new c ->
      apply "new_" [quote_lid c]
  | Pexp_setinstvar (var, expr) ->
      apply "setinstvar" [quote_str var; quote_expression expr]
  | Pexp_override fields ->
      apply "override"
        [quote_list (quote_pair (quote_str) quote_expression)
           fields]
  | Pexp_letmodule (m, me, body) ->
      apply "letmodule"
        [quote_stropt m; quote_module_expr me; quote_expression body]
  | Pexp_letexception (ec, body)
      [@when [%meta ocaml_at_least "4.04.0"]] ->
      apply "letexception"
        [quote_extension_constructor ec; quote_expression body]
  | Pexp_assert expr ->
      apply "assert_" [quote_expression expr]
  | Pexp_lazy expr ->
      apply "lazy_" [quote_expression expr]
  | Pexp_poly (expr, ty) ->
      apply "poly"
        [quote_expression expr; quote_option quote_core_type ty]
  | Pexp_object cs ->
      apply "object_" [quote_class_structure cs]
  | Pexp_newtype (ty, body) ->
      apply "newtype" [quote_str_405 ty; quote_expression body]
  | Pexp_pack me ->
      apply "pack" [quote_module_expr me]
  | Pexp_open [%meta open_pattern] ->
      [%meta apply_open [%e quote_open_declaration] [%e quote_expression]]
  | Pexp_letop { let_; ands; body }
      [@when [%meta ocaml_at_least "4.08.0"]] ->
      apply "letop"
        [quote_binding_op let_; quote_list quote_binding_op ands;
          quote_expression body]
  | Pexp_extension ({ txt = "e"; _ }, payload) ->
      Metapp_utils.expression_of_payload payload
  | Pexp_extension extension ->
      apply "extension" [quote_extension extension]
  | Pexp_unreachable ->
      apply "unreachable" [quote_unit ()]
and quote_case (case : Parsetree.case) : Parsetree.expression =
  Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_exp, "case")))
    (Stdcompat.Option.to_list (Stdcompat.Option.map (fun expr ->
        ("guard", quote_expression expr)) case.pc_guard))
    [quote_pattern case.pc_lhs; quote_expression case.pc_rhs]
and[@when [%meta ocaml_at_least "4.08.0"]]
    quote_binding_op (binding : Parsetree.binding_op) : Parsetree.expression =
  Ast_helper.with_default_loc binding.pbop_loc @@ fun () ->
  Metapp_utils.apply (Metapp_utils.ident (Ldot (ast_helper_exp, "binding_op")))
    [quote_str binding.pbop_op; quote_pattern binding.pbop_pat;
       quote_expression binding.pbop_exp; quote_default_loc ()]
and quote_value_description (desc : Parsetree.value_description)
    : Parsetree.expression =
  Ast_helper.with_default_loc desc.pval_loc @@ fun () ->
  let prims =
    match desc.pval_prim with
    | [] -> []
    | prims -> [("prim", quote_list quote_string prims)] in
  Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_val, "mk")))
    (label_arg_of_attributes desc.pval_attributes @ prims)
    [quote_str desc.pval_name; quote_core_type desc.pval_type]
and quote_type_declaration (decl : Parsetree.type_declaration)
    : Parsetree.expression =
  Ast_helper.with_default_loc decl.ptype_loc @@ fun () ->
  let params =
    match decl.ptype_params with
    | [] -> []
    | params ->
        [("params",
          quote_list (quote_pair quote_core_type quote_variance) params)] in
  let cstrs =
    match decl.ptype_cstrs with
    | [] -> []
    | cstrs ->
        let quote_cstr (t1, t2, _) =
          Ast_helper.Exp.tuple
            [quote_core_type t1; quote_core_type t2; quote_unit ()] in
        [("cstrs", quote_list quote_cstr cstrs)] in
  let kind =
    match decl.ptype_kind with
    | Ptype_abstract -> []
    | kind -> [("kind", quote_type_kind kind)] in
  let priv =
    match decl.ptype_private with
    | Public -> []
    | priv -> [("priv", quote_private_flag priv)] in
  let manifest =
    Stdcompat.Option.to_list (Stdcompat.Option.map
      (fun manifest -> ("manifest", quote_core_type manifest))
      decl.ptype_manifest) in
  Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_type, "mk")))
    (label_arg_of_attributes decl.ptype_attributes @ params @
      cstrs @ kind @ priv @ manifest)
    [quote_str decl.ptype_name]
and quote_type_kind (kind : Parsetree.type_kind) : Parsetree.expression =
  match kind with
  | Ptype_abstract ->
      Metapp_utils.construct (Ldot (parsetree, "Ptype_abstract")) []
  | Ptype_variant cds ->
      Metapp_utils.construct (Ldot (parsetree, "Ptype_variant"))
        [quote_list quote_constructor_declaration cds]
  | Ptype_record lds ->
      Metapp_utils.construct (Ldot (parsetree, "Ptype_record"))
        [quote_list quote_label_declaration lds]
  | Ptype_open ->
      Metapp_utils.construct (Ldot (parsetree, "Ptype_open")) []
and quote_label_declaration (decl : Parsetree.label_declaration)
    : Parsetree.expression =
  Ast_helper.with_default_loc decl.pld_loc @@ fun () ->
  let mutable_flag =
    match decl.pld_mutable with
    | Immutable -> []
    | mut -> [("mut", quote_mutable_flag mut)] in
  Metapp_utils.apply_labels
    (Metapp_utils.ident (Ldot (ast_helper_type, "field")))
    (label_arg_of_attributes decl.pld_attributes @ mutable_flag)
    [quote_str decl.pld_name; quote_core_type decl.pld_type]
and quote_constructor_declaration (decl : Parsetree.constructor_declaration)
    : Parsetree.expression =
  Ast_helper.with_default_loc decl.pcd_loc @@ fun () ->
  let args =
    match decl.pcd_args with
    | Pcstr_tuple [] -> []
    | args -> [("args", quote_constructor_arguments args)] in
  let res =
    match decl.pcd_res with
    | None -> []
    | res -> [("res", quote_option quote_core_type res)] in
  Metapp_utils.apply_labels
    (Metapp_utils.ident (Ldot (ast_helper_type, "constructor")))
    (label_arg_of_attributes decl.pcd_attributes @ args @ res)
    [quote_str decl.pcd_name]
and quote_constructor_arguments (args : Parsetree.constructor_arguments)
    : Parsetree.expression =
  match args with
  | Pcstr_tuple args ->
      Metapp_utils.construct (Ldot (parsetree, "Pcstr_tuple"))
        [quote_list quote_core_type args]
  | Pcstr_record lds ->
      Metapp_utils.construct (Ldot (parsetree, "Pcstr_record"))
        [quote_list quote_label_declaration lds]
and quote_type_extension (e : Parsetree.type_extension) : Parsetree.expression =
  let loc =
    [%meta if Sys.ocaml_version < "4.08.0" then
      [%e e.ptyext_path.loc]
    else
      [%e e.ptyext_loc]] in
  Ast_helper.with_default_loc loc @@ fun () ->
  let params =
    match e.ptyext_params with
    | [] -> []
    | params ->
        [("params",
          quote_list (quote_pair quote_core_type quote_variance) params)] in
  let priv =
    match e.ptyext_private with
    | Public -> []
    | priv -> [("priv", quote_private_flag priv)] in
  Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_te, "mk")))
    (label_arg_of_attributes e.ptyext_attributes @ params @ priv)
    [quote_lid e.ptyext_path;
      quote_list quote_extension_constructor e.ptyext_constructors]
and quote_extension_constructor (cstr : Parsetree.extension_constructor)
    : Parsetree.expression =
  Ast_helper.with_default_loc cstr.pext_loc @@ fun () ->
  Metapp_utils.apply_labels
    (Metapp_utils.ident (Ldot (ast_helper_te, "constructor")))
    (label_arg_of_attributes cstr.pext_attributes)
    [quote_str cstr.pext_name; quote_extension_constructor_kind cstr.pext_kind]
and[@when [%meta ocaml_at_least "4.08.0"]]
    quote_type_exception (tyexn : Parsetree.type_exception)
    : Parsetree.expression =
  Ast_helper.with_default_loc tyexn.ptyexn_loc @@ fun () ->
  Metapp_utils.apply_labels
    (Metapp_utils.ident (Ldot (ast_helper_te, "mk_exception")))
    (label_arg_of_attributes tyexn.ptyexn_attributes)
    [quote_extension_constructor tyexn.ptyexn_constructor]
and quote_extension_constructor_kind
    (kind : Parsetree.extension_constructor_kind) : Parsetree.expression =
  match kind with
  | Pext_decl (args, res) ->
      Metapp_utils.construct (Ldot (parsetree, "Pext_decl"))
        [quote_constructor_arguments args;
          quote_option quote_core_type res]
  | Pext_rebind lid ->
      Metapp_utils.construct (Ldot (parsetree, "Pext_rebind")) [quote_lid lid]
and quote_class_type (ct : Parsetree.class_type) : Parsetree.expression =
  Ast_helper.with_default_loc ct.pcty_loc @@ fun () ->
  let apply f args =
    Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_cty, f)))
      (label_arg_of_attributes ct.pcty_attributes) args in
  match ct.pcty_desc with
  | Pcty_constr (lid, args) ->
      apply "ident" [quote_lid lid; quote_list quote_core_type args]
  | Pcty_signature sgn ->
      apply "signature" [quote_class_signature sgn]
  | Pcty_arrow (label, arg, res) ->
      apply "arrow"
        [quote_arg_label label; quote_core_type arg; quote_class_type res]
  | Pcty_extension e ->
      apply "extension" [quote_extension e]
  | Pcty_open [%meta open_pattern]
      [@when [%meta ocaml_at_least "4.06.0"]] ->
      [%meta apply_open [%e quote_open_description] [%e quote_class_type]]
and quote_class_signature (csig : Parsetree.class_signature)
    : Parsetree.expression =
  Metapp_utils.apply (Metapp_utils.ident (Ldot (ast_helper_csig, "mk")))
    [quote_core_type csig.pcsig_self;
      quote_list quote_class_type_field csig.pcsig_fields]
and quote_class_type_field (ctf : Parsetree.class_type_field)
    : Parsetree.expression =
  Ast_helper.with_default_loc ctf.pctf_loc @@ fun () ->
  let apply f args =
    Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_ctf, f)))
      (label_arg_of_attributes ctf.pctf_attributes) args in
  match ctf.pctf_desc with
  | Pctf_inherit cty ->
      apply "inherit_" [quote_class_type cty]
  | Pctf_val (label, mutable_flag, virtual_flag, ty) ->
      apply "val_"
        [quote_str_405 label; quote_mutable_flag mutable_flag;
          quote_virtual_flag virtual_flag; quote_core_type ty]
  | Pctf_method (label, private_flag, virtual_flag, ty) ->
      apply "method_"
        [quote_str_405 label; quote_private_flag private_flag;
          quote_virtual_flag virtual_flag; quote_core_type ty]
  | Pctf_constraint (t1, t2) ->
      apply "constraint_" [quote_core_type t1; quote_core_type t2]
  | Pctf_attribute attr ->
      apply "attribute" [quote_attribute attr]
  | Pctf_extension e ->
      apply "extension" [quote_extension e]
and quote_class_infos :
    'a . ('a -> Parsetree.expression) -> 'a Parsetree.class_infos ->
      Parsetree.expression =
fun quote_expr infos ->
  Ast_helper.with_default_loc infos.pci_loc @@ fun () ->
  let virt =
    match infos.pci_virt with
    | Concrete -> []
    | virt -> ["virt", quote_virtual_flag virt] in
  let params =
    match infos.pci_params with
    | [] -> []
    | params ->
        [("params",
          quote_list (quote_pair quote_core_type quote_variance) params)] in
  Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_ci, "mk")))
    (label_arg_of_attributes infos.pci_attributes @ virt @ params)
    [quote_str infos.pci_name; quote_expr infos.pci_expr]
and quote_class_description (cd : Parsetree.class_description)
    : Parsetree.expression =
  quote_class_infos quote_class_type cd
and quote_class_type_declaration (ctd : Parsetree.class_type_declaration)
    : Parsetree.expression =
  quote_class_infos quote_class_type ctd
and quote_class_expr (cl : Parsetree.class_expr) : Parsetree.expression =
  Ast_helper.with_default_loc cl.pcl_loc @@ fun () ->
  let apply f args =
    Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_cl, f)))
      (label_arg_of_attributes cl.pcl_attributes) args in
  match cl.pcl_desc with
  | Pcl_constr (lid, args) ->
      apply "constr" [quote_lid lid; quote_list quote_core_type args]
  | Pcl_structure cs ->
      apply "structure" [quote_class_structure cs]
  | Pcl_fun (label, default, pattern, cl) ->
      apply "fun_"
        [quote_arg_label label; quote_option quote_expression default;
          quote_pattern pattern; quote_class_expr cl]
  | Pcl_apply (cl, args) ->
      apply "constraint_"
        [quote_class_expr cl;
          quote_list (quote_pair quote_arg_label quote_expression) args]
  | Pcl_let (rec_flag, bindings, cl) ->
      apply "let_"
        [quote_rec_flag rec_flag; quote_list quote_value_binding bindings;
          quote_class_expr cl]
  | Pcl_constraint (cl, cty) ->
      apply "constraint_" [quote_class_expr cl; quote_class_type cty]
  | Pcl_extension e ->
      apply "extension" [quote_extension e]
  | Pcl_open [%meta open_pattern]
      [@when [%meta ocaml_at_least "4.06.0"]] ->
      [%meta apply_open [%e quote_open_description] [%e quote_class_expr]]
and quote_class_structure (cstr : Parsetree.class_structure)
    : Parsetree.expression =
  Metapp_utils.apply (Metapp_utils.ident (Ldot (ast_helper_cstr, "mk")))
    [quote_pattern cstr.pcstr_self;
      quote_list quote_class_field cstr.pcstr_fields]
and quote_class_field (cf : Parsetree.class_field) : Parsetree.expression =
  Ast_helper.with_default_loc cf.pcf_loc @@ fun () ->
  let apply f args =
    Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_cf, f)))
      (label_arg_of_attributes cf.pcf_attributes) args in
  match cf.pcf_desc with
  | Pcf_inherit (override_flag, cl, var) ->
      apply "inherit_"
        [quote_override_flag override_flag; quote_class_expr cl;
          quote_option quote_str_405 var]
  | Pcf_val (label, mutable_flag, cfk) ->
      apply "val_"
        [quote_str label; quote_mutable_flag mutable_flag;
          quote_class_field_kind cfk]
  | Pcf_method (label, private_flag, cfk) ->
      apply "method_"
        [quote_str label; quote_private_flag private_flag;
          quote_class_field_kind cfk]
  | Pcf_constraint (t1, t2) ->
      apply "constraint_" [quote_core_type t1; quote_core_type t2]
  | Pcf_initializer e ->
      apply "initializer_" [quote_expression e]
  | Pcf_attribute attr ->
      apply "attribute" [quote_attribute attr]
  | Pcf_extension e ->
      apply "extension" [quote_extension e]
and quote_class_field_kind (cfk : Parsetree.class_field_kind)
    : Parsetree.expression =
  match cfk with
  | Cfk_virtual ty ->
      Metapp_utils.construct (Ldot (parsetree, "Cfk_virtual"))
        [quote_core_type ty]
  | Cfk_concrete (override_flag, e) ->
      Metapp_utils.construct (Ldot (parsetree, "Cfk_concrete"))
        [quote_override_flag override_flag; quote_expression e]
and quote_class_declaration (cd : Parsetree.class_declaration)
    : Parsetree.expression =
  quote_class_infos quote_class_expr cd
and quote_module_type (mty : Parsetree.module_type) : Parsetree.expression =
  Ast_helper.with_default_loc mty.pmty_loc @@ fun () ->
  let apply f args =
    Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_mty, f)))
      (label_arg_of_attributes mty.pmty_attributes) args in
  match mty.pmty_desc with
  | Pmty_ident m ->
      apply "ident" [quote_lid m]
  | Pmty_signature s ->
      apply "signature" [quote_signature s]
  | Pmty_functor [%meta functor_pattern] ->
      [%meta apply_functor [%e quote_module_type]]
  | Pmty_with (ty, cstrs) ->
      apply "with_"
        [quote_module_type ty; quote_list quote_with_constraint cstrs]
  | Pmty_typeof e ->
      apply "typeof_" [quote_module_expr e]
  | Pmty_extension ({ txt = "m"; _ }, payload) ->
      Metapp_utils.expression_of_payload payload
  | Pmty_extension e ->
      apply "extension" [quote_extension e]
  | Pmty_alias lid ->
      apply "alias" [quote_lid lid]
and[@when [%meta ocaml_at_least "4.10.0"]]
    quote_functor_parameter (f : Parsetree.functor_parameter)
    : Parsetree.expression =
  match f with
  | Unit ->
      Metapp_utils.construct (Ldot (parsetree, "Unit")) []
  | Named (str, mty) ->
      Metapp_utils.construct (Ldot (parsetree, "Named"))
        [quote_stropt str; quote_module_type mty]
and quote_signature (sign : Parsetree.signature) : Parsetree.expression =
  quote_list quote_signature_item sign
and quote_signature_item (item : Parsetree.signature_item)
    : Parsetree.expression =
  Ast_helper.with_default_loc item.psig_loc @@ fun () ->
  let apply f args =
    Metapp_utils.apply (Metapp_utils.ident (Ldot (ast_helper_sig, f))) args in
  match item.psig_desc with
  | Psig_value vd ->
      apply "value" [quote_value_description vd]
  | Psig_type (rec_flag, decls) ->
      apply "type_"
        [quote_rec_flag rec_flag; quote_list quote_type_declaration decls]
  | Psig_typesubst decls
      [@when [%meta ocaml_at_least "4.08.0"]] ->
      apply "type_subst" [quote_list quote_type_declaration decls]
  | Psig_typext ext ->
      apply "type_extension" [quote_type_extension ext]
  | Psig_exception exc ->
      [%meta quote_exception]
  | Psig_module md ->
      apply "module_" [quote_module_declaration md]
  | Psig_modsubst ms
      [@when [%meta ocaml_at_least "4.08.0"]] ->
      apply "mod_subst" [quote_module_substitution ms]
  | Psig_recmodule mds ->
      apply "rec_module" [quote_list quote_module_declaration mds]
  | Psig_modtype mtd ->
      apply "modtype" [quote_module_type_declaration mtd]
  | Psig_open od ->
      apply "open_" [quote_open_description od]
  | Psig_include id ->
      apply "include_" [quote_include_description id]
  | Psig_class cds ->
      apply "class_" [quote_list quote_class_description cds]
  | Psig_class_type ctds ->
      apply "class_type" [quote_list quote_class_type_declaration ctds]
  | Psig_attribute attribute ->
      apply "attribute" [quote_attribute attribute]
  | Psig_extension (({ txt = "i"; _ }, payload), _) ->
      Metapp_utils.expression_of_payload payload
  | Psig_extension (e, attrs) ->
      Metapp_utils.apply_labels
        (Metapp_utils.ident (Ldot (ast_helper_sig, "extension")))
        (label_arg_of_attributes attrs) [quote_extension e]
and quote_module_declaration (md : Parsetree.module_declaration)
    : Parsetree.expression =
  Ast_helper.with_default_loc md.pmd_loc @@ fun () ->
  Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_md, "mk")))
    (label_arg_of_attributes md.pmd_attributes)
    [quote_stropt md.pmd_name; quote_module_type md.pmd_type]
and[@when [%meta ocaml_at_least "4.08.0"]]
    quote_module_substitution (ms : Parsetree.module_substitution)
    : Parsetree.expression =
  Ast_helper.with_default_loc ms.pms_loc @@ fun () ->
  Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_ms, "mk")))
    (label_arg_of_attributes ms.pms_attributes)
    [quote_str ms.pms_name; quote_lid ms.pms_manifest]
and quote_module_type_declaration (mtd : Parsetree.module_type_declaration)
    : Parsetree.expression =
  Ast_helper.with_default_loc mtd.pmtd_loc @@ fun () ->
  let ty =
    Stdcompat.Option.to_list
      (Stdcompat.Option.map (fun ty -> "typ", quote_module_type ty)
        mtd.pmtd_type) in
  Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_mtd, "mk")))
    (label_arg_of_attributes mtd.pmtd_attributes @ ty)
    [quote_str mtd.pmtd_name]
and[@when [%meta ocaml_at_least "4.08.0"]]
    quote_open_infos : 'a . ('a -> Parsetree.expression) ->
      'a Parsetree.open_infos -> Parsetree.expression =
fun quote_expr infos ->
  Ast_helper.with_default_loc infos.popen_loc @@ fun () ->
  let override =
    match infos.popen_override with
    | Fresh -> []
    | override -> [("override", quote_override_flag override)] in
  Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_opn, "mk")))
    (label_arg_of_attributes infos.popen_attributes @ override)
    [quote_expr infos.popen_expr]
and quote_open_description (od : Parsetree.open_description)
    : Parsetree.expression =
  [%meta if Sys.ocaml_version >= "4.08.0" then
    [%e quote_open_infos quote_lid od]
  else [%e
    Ast_helper.with_default_loc od.popen_loc @@ fun () ->
    let override =
      match od.popen_override with
      | Fresh -> []
      | override -> [("override", quote_override_flag override)] in
    Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_opn, "mk")))
      (label_arg_of_attributes od.popen_attributes @ override)
      [quote_lid od.popen_lid]]]
and[@when [%meta ocaml_at_least "4.08.0"]]
    quote_open_declaration (od : Parsetree.open_declaration)
    : Parsetree.expression =
  quote_open_infos quote_module_expr od
and quote_include_infos :
    'a . ('a -> Parsetree.expression) -> 'a Parsetree.include_infos ->
      Parsetree.expression =
fun quote_mod infos ->
  Ast_helper.with_default_loc infos.pincl_loc @@ fun () ->
  Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_incl, "mk")))
    (label_arg_of_attributes infos.pincl_attributes)
    [quote_mod infos.pincl_mod]
and quote_include_description (od : Parsetree.include_description)
    : Parsetree.expression =
  quote_include_infos quote_module_type od
and quote_include_declaration (od : Parsetree.include_declaration)
    : Parsetree.expression =
  quote_include_infos quote_module_expr od
and quote_with_constraint (wc : Parsetree.with_constraint)
    : Parsetree.expression =
  match wc with
  | Pwith_type (ty, decl) ->
      Metapp_utils.construct (Ldot (parsetree, "Pwith_type"))
        [quote_lid ty; quote_type_declaration decl]
  | Pwith_module (m1, m2) ->
      Metapp_utils.construct (Ldot (parsetree, "Pwith_module"))
        [quote_lid m1; quote_lid m2]
  | Pwith_typesubst (ty [@when [%meta ocaml_at_least "4.06.0"]], decl) ->
      Metapp_utils.construct (Ldot (parsetree, "Pwith_typesubst"))
      (((List.cons (quote_lid ty)) [@when [%meta ocaml_at_least "4.06.0"]])
          [quote_type_declaration decl])
  | Pwith_modsubst (m1, m2) ->
      Metapp_utils.construct (Ldot (parsetree, "Pwith_modsubst"))
        [[%meta if Sys.ocaml_version >= "4.06.0" then
          [%e quote_lid]
        else
          [%e quote_str]] m1; quote_lid m2]
and quote_module_expr (e : Parsetree.module_expr) : Parsetree.expression =
  Ast_helper.with_default_loc e.pmod_loc @@ fun () ->
  let apply f args =
    Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_mod, f)))
      (label_arg_of_attributes e.pmod_attributes) args in
  match e.pmod_desc with
  | Pmod_ident m ->
      apply "ident" [quote_lid m]
  | Pmod_structure s ->
      apply "structure" [quote_structure s]
  | Pmod_functor [%meta functor_pattern] ->
      [%meta apply_functor [%e quote_module_expr]]
  | Pmod_apply (f, e) ->
      apply "apply" [quote_module_expr f; quote_module_expr e]
  | Pmod_constraint (e, ty) ->
      apply "constraint_" [quote_module_expr e; quote_module_type ty]
  | Pmod_unpack e ->
      apply "unpack" [quote_expression e]
  | Pmod_extension ({ txt = "m"; _ }, payload) ->
      Metapp_utils.expression_of_payload payload
  | Pmod_extension e ->
      apply "extension" [quote_extension e]
and quote_structure (sign : Parsetree.structure) : Parsetree.expression =
  quote_list quote_structure_item sign
and quote_structure_item (item : Parsetree.structure_item)
    : Parsetree.expression =
  Ast_helper.with_default_loc item.pstr_loc @@ fun () ->
  let apply f args =
    Metapp_utils.apply (Metapp_utils.ident (Ldot (ast_helper_str, f))) args in
  match item.pstr_desc with
  | Pstr_eval (e, attrs) ->
      Metapp_utils.apply_labels
        (Metapp_utils.ident (Ldot (ast_helper_str, "eval")))
        (label_arg_of_attributes attrs) [quote_expression e]
  | Pstr_value (rec_flag, vbs) ->
      apply "value"
        [quote_rec_flag rec_flag; quote_list quote_value_binding vbs]
  | Pstr_primitive vd ->
      apply "primitive" [quote_value_description vd]
  | Pstr_type (rec_flag, decls) ->
      apply "type_"
        [quote_rec_flag rec_flag; quote_list quote_type_declaration decls]
  | Pstr_typext ext ->
      apply "type_extension" [quote_type_extension ext]
  | Pstr_exception exc ->
      [%meta quote_exception]
  | Pstr_module mb ->
      apply "module_" [quote_module_binding mb]
  | Pstr_recmodule mbs ->
      apply "rec_module" [quote_list quote_module_binding mbs]
  | Pstr_modtype mtd ->
      apply "modtype" [quote_module_type_declaration mtd]
  | Pstr_open od ->
      apply "open_" [[%meta if Sys.ocaml_version >= "4.08.0" then
        [%e quote_open_declaration od]
      else
        [%e quote_open_description od]]]
  | Pstr_class cds ->
      apply "class_" [quote_list quote_class_declaration cds]
  | Pstr_class_type ctds ->
      apply "class_type" [quote_list quote_class_type_declaration ctds]
  | Pstr_include id ->
      apply "include_" [quote_include_declaration id]
  | Pstr_attribute attribute ->
      apply "attribute" [quote_attribute attribute]
  | Pstr_extension (({ txt = "i"; _ }, payload), _) ->
      Metapp_utils.expression_of_payload payload
  | Pstr_extension (e, attrs) ->
      Metapp_utils.apply_labels
        (Metapp_utils.ident (Ldot (ast_helper_str, "extension")))
        (label_arg_of_attributes attrs) [quote_extension e]
and quote_value_binding (binding : Parsetree.value_binding)
    : Parsetree.expression =
  Ast_helper.with_default_loc binding.pvb_loc @@ fun () ->
  Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_vb, "mk")))
    (label_arg_of_attributes binding.pvb_attributes)
    [quote_pattern binding.pvb_pat; quote_expression binding.pvb_expr]
and quote_module_binding (binding : Parsetree.module_binding)
    : Parsetree.expression =
  Ast_helper.with_default_loc binding.pmb_loc @@ fun () ->
  Metapp_utils.apply_labels (Metapp_utils.ident (Ldot (ast_helper_mb, "mk")))
    (label_arg_of_attributes binding.pmb_attributes)
    [quote_stropt binding.pmb_name; quote_module_expr binding.pmb_expr]
and quote_extension (extension : Parsetree.extension) : Parsetree.expression =
  quote_pair quote_str quote_payload extension
and quote_attribute (attribute : Parsetree.attribute) : Parsetree.expression =
  [%meta if Sys.ocaml_version >= "4.08.0" then [%e
    Ast_helper.with_default_loc attribute.attr_loc @@ fun () ->
    Metapp_utils.apply (Metapp_utils.ident (Ldot (ast_helper_attr, "mk")))
      [quote_str attribute.attr_name; quote_payload attribute.attr_payload]]
  else
    [%e quote_pair quote_str quote_payload attribute]]
and quote_attributes (attributes : Parsetree.attributes)
    : Parsetree.expression =
  quote_list quote_attribute attributes
and quote_payload (payload : Parsetree.payload) : Parsetree.expression =
  match payload with
  | PStr str ->
      Metapp_utils.construct (Ldot (parsetree, "PStr")) [quote_structure str]
  | PSig sign ->
      Metapp_utils.construct (Ldot (parsetree, "PSig")) [quote_signature sign]
  | PTyp ty ->
      Metapp_utils.construct (Ldot (parsetree, "PTyp")) [quote_core_type ty]
  | PPat (pat, when_) ->
      Metapp_utils.construct (Ldot (parsetree, "PPat"))
        [quote_pattern pat; quote_option quote_expression when_]
and label_arg_of_attributes attributes =
  match attributes with
  | [] -> []
  | _ -> ["attrs", quote_attributes attributes]
and expr (mapper : Ast_mapper.mapper) (e : Parsetree.expression)
    : Parsetree.expression =
  Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match e.pexp_desc with
  | Pexp_extension ({ txt = "expr"; _ }, payload) ->
      quote_expression (Metapp_utils.expression_of_payload payload)
  | Pexp_extension ({ txt = "pat"; _ }, payload) ->
      quote_pattern (Metapp_utils.pattern_of_payload payload)
  | Pexp_extension ({ txt = "str"; _ }, payload) ->
      quote_structure (Metapp_utils.structure_of_payload payload)
  | Pexp_extension ({ txt = "stri"; _ }, payload) ->
      quote_structure_item (Metapp_utils.structure_item_of_payload payload)
  | Pexp_extension ({ txt = "sig"; _ }, payload) ->
      quote_signature (Metapp_utils.signature_of_payload payload)
  | Pexp_extension ({ txt = "sigi"; _ }, payload) ->
      quote_signature_item (Metapp_utils.signature_item_of_payload payload)
  | Pexp_extension ({ txt = "type"; _ }, payload) ->
      quote_core_type (Metapp_utils.core_type_of_payload payload)
  | _ -> Ast_mapper.default_mapper.expr mapper e
and mapper : Ast_mapper.mapper = {
  Ast_mapper.default_mapper with expr
}]]

let rewriter _config _cookies : Ast_mapper.mapper =
  mapper

let () =
  Migrate_parsetree.Driver.register ~name:"metaquot"
    (module Migrate_parsetree.OCaml_current)
    rewriter
