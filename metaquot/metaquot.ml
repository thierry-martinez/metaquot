[%%metapackage "metapp.utils"]
[%%metapackage "findlib"]
[%%metaflags ["-open"; "Stdcompat"]]

let expression_of_default_loc () : Parsetree.expression =
  Metapp_utils.apply (Metapp_utils.ident (Lident "!"))
    [Metapp_utils.ident (Ldot (Lident "Ast_helper", "default_loc"))]

module type QuoteValueS = sig
  include Metapp_utils.ValueS

  val quote_location : Location.t -> t

  (* OCaml >= 4.10.0 *)
  val quote_location_stack : _ -> t
end

module QuoteExp : QuoteValueS with type t = Parsetree.expression = struct
  include Metapp_utils.Exp

  let quote_location (_ : Location.t) : Parsetree.expression =
    expression_of_default_loc ()

  let quote_location_stack (_ : _) : Parsetree.expression =
    Metapp_utils.Exp.nil ()
end

module QuotePat : QuoteValueS with type t = Parsetree.pattern = struct
  include Metapp_utils.Pat

  let quote_location (_ : Location.t) : Parsetree.pattern =
    Ast_helper.Pat.any ()

  let quote_location_stack (_ : _) : Parsetree.pattern =
    Ast_helper.Pat.any ()
end

[%%metadef
let parsetree = Longident.Lident "Parsetree"

let asttypes = Longident.Lident "Asttypes"

let quote_name name =
  Printf.sprintf "%s" name

let quote_of_path (path : Path.t) : Parsetree.expression =
  let name =
    match Untypeast.lident_of_path path with
    | Lident name | Ldot (Lident "Asttypes", name) -> name
    | Ldot (Lident "Location", "t") -> "location"
    | Ldot (Lident "Longident", "t") -> "longident"
    | lident ->
        failwith (Format.asprintf "quote_of_path: %s"
          (String.concat "." (Longident.flatten lident))) in
  Metapp_utils.Exp.var (quote_name name)

let index_variables args =
  List.mapi (fun i arg -> Printf.sprintf "x%d" i, arg) args

let rec quote_of_type_expr (ty : Types.type_expr) : Parsetree.expression =
  match ty.desc with
  | Tvar x ->
      Metapp_utils.Exp.var (quote_name (Option.get x))
  | Tconstr (path, [], _) ->
      quote_of_path path
  | Tconstr (path, args, _) ->
      Metapp_utils.apply (quote_of_path path) (List.map quote_of_type_expr args)
  | Ttuple args ->
      let args = index_variables args in
      let pat =
        Metapp_utils.Pat.tuple
          (List.map (fun (x, _) -> Metapp_utils.Pat.var x) args) in
      let exp =
        Metapp_utils.apply
          (Metapp_utils.ident (Longident.Ldot (Lident "Target", "tuple")))
          [Metapp_utils.Exp.of_list
            (args |> List.map (fun (x, arg) ->
              Metapp_utils.apply (quote_of_type_expr arg)
                [Metapp_utils.Exp.var x]))] in
      Ast_helper.Exp.fun_ Nolabel None pat exp
  | _ ->
      assert false

let case_of_ctor (prefix : Longident.t)
    (declaration : Types.constructor_declaration) : Parsetree.case =
  let args =
    match declaration.cd_args with
    | Cstr_tuple args -> args
    | _ -> assert false in
  let args = index_variables args in
  let name = Ident.name declaration.cd_id in
  let pat =
    Metapp_utils.Pat.construct (Lident name)
      (List.map (fun (x, _) -> Metapp_utils.Pat.var x) args) in
  let exp =
    Metapp_utils.apply
      (Metapp_utils.ident (Longident.Ldot (Lident "Target", "construct")))
      [Metapp_utils.Exp.of_longident (Ldot (prefix, name));
        Metapp_utils.Exp.of_list
          (args |> List.map (fun (x, arg) ->
            Metapp_utils.apply (quote_of_type_expr arg)
              [Metapp_utils.Exp.var x]))] in
  Ast_helper.Exp.case pat exp

let quote_of_record (prefix : Longident.t)
    (labels : Types.label_declaration list) : Parsetree.expression =
  let labels = index_variables labels in
  let pat =
    Metapp_utils.Pat.record (labels |> List.map
      (fun (x, (label : Types.label_declaration)) ->
        (Longident.Lident (Ident.name label.ld_id),
          Metapp_utils.Pat.var x))) in
  let exp =
    Metapp_utils.apply
      (Metapp_utils.ident (Longident.Ldot (Lident "Target", "record")))
      [Metapp_utils.Exp.of_list
        (labels |> List.map (fun (x, (label : Types.label_declaration)) ->
          Metapp_utils.Exp.tuple [
            Metapp_utils.Exp.of_longident
              (Ldot (prefix, Ident.name label.ld_id));
            Metapp_utils.apply (quote_of_type_expr label.ld_type)
              [Metapp_utils.Exp.var x]]))] in
  Ast_helper.Exp.fun_ Nolabel None pat exp

let quote_of_declaration (prefix : Longident.t) (name : string)
    (declaration : Types.type_declaration) : Parsetree.value_binding =
  let exp =
    match declaration.type_kind with
    | Type_abstract ->
        Ast_helper.Exp.fun_ Nolabel None (Metapp_utils.Pat.var "x")
          (Metapp_utils.apply
            (quote_of_type_expr (Option.get declaration.type_manifest))
            [Metapp_utils.Exp.var "x"])
    | Type_variant ctors ->
        Ast_helper.Exp.function_ (List.map (case_of_ctor prefix) ctors)
    | Type_record (labels, _) ->
        quote_of_record prefix labels
    | Type_open -> assert false in
  let target =
    Ast_helper.Typ.constr
      (Metapp_utils.loc (Longident.Ldot (Lident "Target", "t"))) [] in
  let param_names =
    declaration.type_params |> List.map (fun (ty : Types.type_expr) ->
      match ty.desc with
      | Tvar (Some name) -> name
      | _ -> assert false) in
  let typ =
    Ast_helper.Typ.arrow Nolabel
      (Ast_helper.Typ.constr (Metapp_utils.loc (Longident.Ldot (prefix, name)))
        (List.map Ast_helper.Typ.var param_names))
      target in
  let add_param ty typ =
    Ast_helper.Typ.arrow Nolabel
      (Ast_helper.Typ.arrow Nolabel (Ast_helper.Typ.var ty) target) typ in
  let typ = List.fold_right add_param param_names typ in
  let typ =
    match param_names with
    | [] -> typ
    | _ -> Metapp_utils.Typ.poly param_names typ in
  let add_param name exp =
    Ast_helper.Exp.fun_ Nolabel None (Metapp_utils.Pat.var (quote_name name))
      exp in
  let exp = List.fold_right add_param param_names exp in
  let pat =
    Ast_helper.Pat.constraint_ (Metapp_utils.Pat.var (quote_name name)) typ in
  Ast_helper.Vb.mk pat exp

let quote_of_sig (filter : string list -> bool) (prefix : Longident.t)
    (s : Types.signature) : Parsetree.structure_item =
  let accu_group group accu =
    match group with
    | None -> accu
    | Some (rec_flag, group) -> (rec_flag, List.rev group) :: accu in
  let add_item (group, accu) (item : Types.signature_item) =
    match Metapp_utils.destruct_sig_type item with
    | Some { id; decl; rec_status; _ } ->
        let ((rec_status, accu_group), accu) =
          match (rec_status, group) with
          | (Trec_next, Some group) -> (group, accu)
          | (Trec_first, _) -> ((Asttypes.Recursive, []), accu_group group accu)
          | (Trec_not, _) ->
              ((Asttypes.Nonrecursive, []), accu_group group accu)
          | _ -> assert false in
        (Some (rec_status, (id, decl) :: accu_group), accu)
    | None -> (group, accu) in
  let (group, accu) = List.fold_left add_item (None, []) s in
  let groups = List.rev (accu_group group accu) in
  let groups = groups |> List.filter (fun (_, declarations) ->
    filter (declarations |> List.map (fun (id, _) -> Ident.name id))) in
  let s = groups |> List.map (fun (rec_flag, declarations) ->
    Ast_helper.Str.value rec_flag
      (List.map (fun (id, decl) ->
        quote_of_declaration prefix (Ident.name id) decl)
        declarations)) in
  Metapp_utils.include_structure s

let () = Findlib.init ()

let compiler_libs = Findlib.package_directory "compiler-libs"

let signature_of_cmi filename =
  (Cmi_format.read_cmi (Filename.concat compiler_libs filename)).cmi_sign

let asttypes_signature = signature_of_cmi "asttypes.cmi"

let parsetree_signature = signature_of_cmi "parsetree.cmi"
]

module Quoter (Target : QuoteValueS) = struct
  let unit = Target.of_unit

  let string s = Target.of_string s

  let char = Target.of_char

  let location = Target.quote_location

  let location_stack = Target.quote_location_stack

  let bool = Target.of_bool

  let longident = Target.of_longident

  let list f l =
    Target.of_list (List.map f l)

  let option (quote_value : 'a -> Target.t) (option : 'a option)
      : Target.t =
    Target.of_option (Stdcompat.Option.map quote_value option)

  [%%meta
     quote_of_sig (fun names -> not (List.mem "constant" names)) asttypes
       asttypes_signature]

  [%%meta
     quote_of_sig (fun names ->
       List.mem "constant" names || List.mem "core_type" names) parsetree
       parsetree_signature]

  let quote_extension (name : string) (payload : Parsetree.payload)
      : Target.t option =
    match name with
    | "expr" ->
        Some (expression (Metapp_utils.expression_of_payload payload))
    | "pat" ->
        Some (pattern (Metapp_utils.pattern_of_payload payload))
    | "str" ->
        Some (structure (Metapp_utils.structure_of_payload payload))
    | "stri" ->
        Some (structure_item (Metapp_utils.structure_item_of_payload payload))
    | "sig" ->
        Some (signature (Metapp_utils.signature_of_payload payload))
    | "sigi" ->
        Some (signature_item
          (Metapp_utils.signature_item_of_payload payload))
    | "type" ->
        Some (core_type (Metapp_utils.core_type_of_payload payload))
    | _ -> None
end

let expr (mapper : Ast_mapper.mapper) (e : Parsetree.expression)
    : Parsetree.expression =
  let module Quoter = Quoter (QuoteExp) in
  Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match e.pexp_desc with
  | Pexp_extension ({ txt; _ }, payload) ->
      begin match Quoter.quote_extension txt payload with
      | None -> Ast_mapper.default_mapper.expr mapper e
      | Some e -> e
      end
  | _ -> Ast_mapper.default_mapper.expr mapper e

let pat (mapper : Ast_mapper.mapper) (p : Parsetree.pattern)
    : Parsetree.pattern =
  let module Quoter = Quoter (QuotePat) in
  Ast_helper.with_default_loc p.ppat_loc @@ fun () ->
  match p.ppat_desc with
  | Ppat_extension ({ txt; _ }, payload) ->
      begin match Quoter.quote_extension txt payload with
      | None -> Ast_mapper.default_mapper.pat mapper p
      | Some p -> p
      end
  | _ -> Ast_mapper.default_mapper.pat mapper p

let mapper : Ast_mapper.mapper = { Ast_mapper.default_mapper with expr; pat }

let rewriter _config _cookies : Ast_mapper.mapper =
  mapper

let () =
  Migrate_parsetree.Driver.register ~name:"metaquot"
    (module Migrate_parsetree.OCaml_current)
    rewriter
