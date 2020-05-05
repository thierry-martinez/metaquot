(** This module does not have an associated .mli file because its
signature depends dynamically from the contents of [Parsetree].

The module mainly provides two sub-modules [Metaquot.Exp] and
[Metaquot.Pat], which provides lifters for expressions and patterns
respectively. *)

[%%metapackage metapp, findlib]
[%%metaflag "-open", "Stdcompat"]

let expression_of_default_loc () : Parsetree.expression =
  Metapp.apply (Metapp.ident (Lident "!"))
    [Metapp.ident (Ldot (Lident "Ast_helper", "default_loc"))]

module type QuoteValueS = sig
  include Metapp.ValueS

  val quote_location : Location.t -> t

  val quote_location_stack : _ -> t
end

module QuoteExp : QuoteValueS with type t = Parsetree.expression = struct
  include Metapp.Exp

  let quote_location (_ : Location.t) : Parsetree.expression =
    expression_of_default_loc ()

  let quote_location_stack (_ : _) : Parsetree.expression =
    Metapp.Exp.nil ()
end

module QuotePat : QuoteValueS with type t = Parsetree.pattern = struct
  include Metapp.Pat

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
  Metapp.Exp.var (quote_name name)

let index_variables args =
  List.mapi (fun i arg -> Printf.sprintf "x%d" i, arg) args

let rec quote_of_type_expr (ty : Types.type_expr) : Parsetree.expression =
  match ty.desc with
  | Tvar x ->
      Metapp.Exp.var (quote_name (Option.get x))
  | Tconstr (path, [], _) ->
      quote_of_path path
  | Tconstr (path, args, _) ->
      Metapp.apply (quote_of_path path) (List.map quote_of_type_expr args)
  | Ttuple args ->
      let args = index_variables args in
      let pat =
        Metapp.Pat.tuple
          (List.map (fun (x, _) -> Metapp.Pat.var x) args) in
      let exp =
        Metapp.apply
          (Metapp.ident (Longident.Ldot (Lident "Target", "tuple")))
          [Metapp.Exp.list
            (args |> List.map (fun (x, arg) ->
              Metapp.apply (quote_of_type_expr arg)
                [Metapp.Exp.var x]))] in
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
    Metapp.Pat.construct (Lident name)
      (List.map (fun (x, _) -> Metapp.Pat.var x) args) in
  let exp =
    [%e Target.construct
       [%meta Metapp.Exp.of_longident (Ldot (prefix, name))]
       [%meta Metapp.Exp.list
          (args |> List.map (fun (x, arg) ->
            Metapp.apply (quote_of_type_expr arg)
              [Metapp.Exp.var x]))]] in
  Ast_helper.Exp.case pat exp

let quote_of_record (prefix : Longident.t)
    (labels : Types.label_declaration list) : Parsetree.case=
  let labels = index_variables labels in
  let pat =
    Metapp.Pat.record (labels |> List.map
      (fun (x, (label : Types.label_declaration)) ->
        (Longident.Lident (Ident.name label.ld_id),
          Metapp.Pat.var x))) in
  let exp =
    Metapp.apply
      (Metapp.ident (Longident.Ldot (Lident "Target", "record")))
      [Metapp.Exp.list
        (labels |> List.map (fun (x, (label : Types.label_declaration)) ->
          let name = Ident.name label.ld_id in
          let value =
            match name with
            | "pexp_loc_stack" | "ppat_loc_stack" ->
                [%e Target.quote_location_stack ()]
            | _ ->
                Metapp.apply (quote_of_type_expr label.ld_type)
                  [Metapp.Exp.var x] in
          Metapp.Exp.tuple [
            Metapp.Exp.of_longident
              (Ldot (prefix, name));
            value]))] in
  Ast_helper.Exp.case pat exp

let quote_of_declaration (prefix : Longident.t) (name : string)
    (declaration : Types.type_declaration) : Parsetree.value_binding =
  let cases =
    match declaration.type_kind with
    | Type_abstract ->
        [Ast_helper.Exp.case (Metapp.Pat.var "x")
          (Metapp.apply
            (quote_of_type_expr (Option.get declaration.type_manifest))
            [Metapp.Exp.var "x"])]
    | Type_variant ctors ->
        List.map (case_of_ctor prefix) ctors
    | Type_record (labels, _) ->
        [quote_of_record prefix labels]
    | Type_open -> assert false in
  let pat =
    match name with
    | "core_type" ->
        Some [%p? { ptyp_desc = Ptyp_extension ({ txt = "t"; _ }, payload); _ }]
    | "pattern" ->
        Some [%p? { ppat_desc = Ppat_extension ({ txt = "p"; _ }, payload); _ }]
    | "expression" ->
        Some [%p? { pexp_desc = Pexp_extension ({ txt = "e"; _ }, payload); _ }]
    | "module_type" ->
        Some [%p? { pmty_desc = Pmty_extension ({ txt = "m"; _ }, payload); _ }]
    | "module_expr" ->
        Some [%p? { pmod_desc = Pmod_extension ({ txt = "m"; _ }, payload); _ }]
    | "signature_item" ->
        Some [%p?
          { psig_desc = Psig_extension (({ txt = "i"; _ }, payload), _); _ }]
    | "structure_item" ->
        Some [%p?
          { pstr_desc = Pstr_extension (({ txt = "i"; _ }, payload), _); _ }]
    | _ -> None in
  let cases =
    match pat with
    | None -> cases
    | Some pat ->
        Ast_helper.Exp.case pat [%e
          Target.mapper.get Mapper.mapper Mapper.mapper
            (Target.of_payload payload)] :: cases in
  let exp = Ast_helper.Exp.function_ cases in
  let target =
    Ast_helper.Typ.constr
      (Metapp.mkloc (Longident.Ldot (Lident "Target", "t"))) [] in
  let param_names =
    declaration.type_params |> List.map (fun (ty : Types.type_expr) ->
      match ty.desc with
      | Tvar (Some name) -> name
      | _ -> assert false) in
  let typ =
    Ast_helper.Typ.arrow Nolabel
      (Ast_helper.Typ.constr (Metapp.mkloc
        (Longident.Ldot (prefix, name)))
        (List.map Ast_helper.Typ.var param_names))
      target in
  let add_param ty typ =
    Ast_helper.Typ.arrow Nolabel
      (Ast_helper.Typ.arrow Nolabel (Ast_helper.Typ.var ty) target) typ in
  let typ = List.fold_right add_param param_names typ in
  let typ =
    match param_names with
    | [] -> typ
    | _ -> Metapp.Typ.poly (List.map Metapp.mkloc param_names) typ in
  let add_param name exp =
    Ast_helper.Exp.fun_ Nolabel None (Metapp.Pat.var (quote_name name))
      exp in
  let exp = List.fold_right add_param param_names exp in
  let pat =
    Ast_helper.Pat.constraint_ (Metapp.Pat.var (quote_name name)) typ in
  Ast_helper.Vb.mk pat exp

let quote_of_sig (filter : string list -> bool) (prefix : Longident.t)
    (s : Types.signature) : Parsetree.structure_item =
  let accu_group group accu =
    match group with
    | None -> accu
    | Some (rec_flag, group) -> (rec_flag, List.rev group) :: accu in
  let add_item (group, accu) (item : Types.signature_item) =
    match Metapp.destruct_sig_type item with
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
  Metapp.Stri.of_list s

let () = Findlib.init ()

let compiler_libs = Findlib.package_directory "compiler-libs"

let signature_of_cmi filename =
  (Cmi_format.read_cmi (Filename.concat compiler_libs filename)).cmi_sign

let asttypes_signature = signature_of_cmi "asttypes.cmi"

let parsetree_signature = signature_of_cmi "parsetree.cmi"
]

module type MapperS = sig
  val mapper : Ast_mapper.mapper
end

module DefaultMapper = struct
  let mapper = Ast_mapper.default_mapper
end

module Make (Target : QuoteValueS) = struct
  module Quoter (Mapper : MapperS) = struct
    let unit = Target.of_unit

    let string s = Target.of_string s

    let char = Target.of_char

    let location = Target.quote_location

    let location_stack = Target.quote_location_stack

    let bool = Target.of_bool

    let longident = Target.of_longident

    let list f l =
      Target.list (List.map f l)

    let option (quote_value : 'a -> Target.t) (option : 'a option)
        : Target.t =
      Target.option (Option.map quote_value option)

    [%%meta
       quote_of_sig (fun names -> not (List.mem "constant" names)) asttypes
         asttypes_signature]

    [%%meta
       quote_of_sig (fun names ->
         List.mem "constant" names || List.mem "core_type" names) parsetree
         parsetree_signature]

    let quote_extension (({ txt; loc }, payload) : Parsetree.extension)
        : Target.t option =
      Ast_helper.with_default_loc loc @@ fun () ->
      match txt with
      | "expr" ->
          Some (expression (Metapp.Exp.of_payload payload))
      | "pat" ->
          Some (pattern (Metapp.Pat.of_payload payload))
      | "str" ->
          Some (structure (Metapp.Str.of_payload payload))
      | "stri" ->
          Some (structure_item (Metapp.Stri.of_payload payload))
      | "sig" ->
          Some (signature (Metapp.Sig.of_payload payload))
      | "sigi" ->
          Some (signature_item (Metapp.Sigi.of_payload payload))
      | "type" ->
          Some (core_type (Metapp.Typ.of_payload payload))
      | "lid" ->
          Some (longident (Metapp.longident_of_payload payload))
      | _ -> None
  end

  let lift (mapper : Ast_mapper.mapper) (e : Target.t) : Target.t =
    let module Mapper = struct
      let mapper = mapper
    end in
    let module Quoter = Quoter (Mapper) in
    Ast_helper.with_default_loc (Target.to_loc e) @@ fun () ->
    match Option.bind (Target.destruct_extension e) Quoter.quote_extension with
    | Some e -> e
    | None -> Target.mapper.get Ast_mapper.default_mapper mapper e

  include Quoter (DefaultMapper)
end

module Exp = Make (QuoteExp)

module Pat = Make (QuotePat)
