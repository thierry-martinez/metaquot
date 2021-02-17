(** This module does not have an associated .mli file because its
signature depends dynamically from the contents of [Parsetree].

The module mainly provides two sub-modules [Metaquot.Exp] and
[Metaquot.Pat], which provides lifters for expressions and patterns
respectively. *)

[%%metapackage metapp, findlib]
[%%metaflag "-open", "Stdcompat"]

let expression_of_default_loc () : Ppxlib.expression =
  Metapp.apply (Metapp.Exp.var "!")
    [Metapp.Exp.ident
       (Ldot (Ldot (Lident "Ppxlib", "Ast_helper"), "default_loc"))]

type mapper = {
    expression : Ppxlib.expression -> Ppxlib.expression;
    pattern : Ppxlib.pattern -> Ppxlib.pattern;
  }

module Eq = struct
  type ('a, 'b) t = Refl : ('a, 'a) t
end

module type QuoteValueS = sig
  include Metapp.ValueS

  val quote_location : Location.t -> t

  val quote_location_stack : _ -> t

  val subst_of_expr : Ppxlib.expression -> t

  val is_expr : (t, Ppxlib.expression) Eq.t option

  val get_mapper : mapper -> t -> t
end

module QuoteExp : QuoteValueS with type t = Ppxlib.expression = struct
  include Metapp.Exp

  let quote_location (_ : Location.t) : Ppxlib.expression =
    expression_of_default_loc ()

  let quote_location_stack (_ : _) : Ppxlib.expression =
    Metapp.Exp.nil ()

  let subst_of_expr e = e

  let is_expr = Some Eq.Refl

  let get_mapper mapper = mapper.expression
end

module QuotePat : QuoteValueS with type t = Ppxlib.pattern = struct
  include Metapp.Pat

  let quote_location (_ : Location.t) : Ppxlib.pattern =
    Ppxlib.Ast_helper.Pat.any ()

  let quote_location_stack (_ : _) : Ppxlib.pattern =
    Ppxlib.Ast_helper.Pat.any ()

  let subst_of_expr (e : Ppxlib.expression) =
    match e with
    | { pexp_desc = Pexp_ident { txt = Lident txt; loc }} ->
        Ppxlib.Ast_helper.Pat.var { txt; loc }
    | { pexp_loc; _ } ->
        Location.raise_errorf ~loc:pexp_loc "Simple variable expected"

  let is_expr = None

  let get_mapper mapper = mapper.pattern
end

[%%metadef
let ppxlib = Longident.Lident "Ppxlib"

let asttypes = Longident.Ldot (ppxlib, "Asttypes")

let find_module module_name (signature : Types.signature) :
    Types.signature option =
  signature |> List.find_map (fun (item : Types.signature_item) ->
    match item with
    | Sig_module (ident, _, { md_type = Mty_signature s; _ }, _, _)
      when Ident.name ident = module_name -> Some s
    | _ -> None)

let quote_name name =
  Printf.sprintf "%s" name

let quote_of_path (path : Path.t) : Ppxlib.expression =
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

let rec quote_of_type_expr (ty : Types.type_expr) : Ppxlib.expression =
  match ty.desc with
  | Tvar x ->
      Metapp.Exp.var (quote_name (Option.get x))
  | Tconstr (Pident list, [arg], _) when Ident.name list = "list" ->
      Metapp.apply (Metapp.Exp.var (quote_name "list"))
        ~labels:["subst", [%e subst]] ~optional:["in_list", [%e in_list]]
        [Ppxlib.Ast_helper.Exp.fun_ (Labelled "subst") None (Metapp.Pat.var "subst")
          (Ppxlib.Ast_helper.Exp.fun_ (Optional "in_list") None (Metapp.Pat.var "in_list")
            (quote_of_type_expr arg))]
  | Tconstr (path, args, _) ->
      Metapp.apply (quote_of_path path)
        ~labels:["subst", [%e subst]] ~optional:["in_list", [%e in_list]]
        (List.map quote_of_type_expr args)
  | Ttuple args ->
      let args = index_variables args in
      let pat =
        Metapp.Pat.tuple
          (List.map (fun (x, _) -> Metapp.Pat.var x) args) in
      let exp =
        Metapp.apply
          (Metapp.Exp.ident (Ldot (Lident "Target", "tuple")))
          [Metapp.Exp.list
            (args |> List.map (fun (x, arg) ->
              Metapp.apply (quote_of_type_expr arg)
                [Metapp.Exp.var x]))] in
      Ppxlib.Ast_helper.Exp.fun_ Nolabel None pat exp
  | _ ->
      assert false

let case_of_ctor (prefix : Longident.t)
    (declaration : Types.constructor_declaration) : Ppxlib.case =
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
  Ppxlib.Ast_helper.Exp.case pat exp

let quote_of_record (prefix : Longident.t)
    (labels : Types.label_declaration list) : Ppxlib.case =
  let labels = index_variables labels in
  let pat =
    Metapp.Pat.record (labels |> List.map
      (fun (x, (label : Types.label_declaration)) ->
        (Longident.Lident (Ident.name label.ld_id),
          Metapp.Pat.var x))) in
  let exp =
    Metapp.apply
      (Metapp.Exp.ident (Ldot (Lident "Target", "record")))
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
  let exp =
    match labels |> List.find_map (fun (x, (label : Types.label_declaration)) ->
      match label.ld_type with
      | { desc = Tconstr (Pident ident, [], _)}
        when Ident.name ident = "attributes" -> Some x
      | _ -> None) with
    | None -> exp
    | Some attributes ->
        [%e let subst, [%meta Metapp.Pat.var attributes] =
          match Metapp.Attr.chop "subst" [%meta Metapp.Exp.var attributes] with
          | None -> subst, [%meta Metapp.Exp.var attributes]
          | Some (attribute, attributes) ->
              StringMap.union (fun _ _ x -> Some x)
                subst (parse_subst attribute), attributes in
        let [%meta Metapp.Pat.var attributes], k =
          match Metapp.Attr.chop "for" [%meta Metapp.Exp.var attributes] with
          | None -> [%meta Metapp.Exp.var attributes], Fun.id
          | Some (attribute, attributes) ->
              match Metapp.Exp.of_payload (Metapp.Attr.payload attribute) with
              | [%expr [%e? target] := [%e? list]] ->
                  let loc = Metapp.Attr.to_loc attribute in
                  begin match in_list with
                  | None ->
                      Location.raise_errorf ~loc
                        "@for attribute is only allowed in lists"
                  | Some in_list ->
                      let list_index = !list_counter in
                      list_counter := succ list_index;
                      let list_identifier =
                        Printf.sprintf "list%d" list_index in
                      in_list := list_identifier :: !in_list;
                      let k (e : Target.t) : Target.t =
                        match Target.is_expr with
                        | None ->
                      Location.raise_errorf ~loc
                        "@for attribute is only allowed in expressions"
                        | Some Refl ->
                        [%expr
                           let list =
                             match ![%e Metapp.Exp.var list_identifier] with
                             | None -> [%e list]
                             | Some list -> list in
                           match list with
                           | [] -> raise End_of_list
                           | hd :: tl ->
                               [%e Metapp.Exp.var list_identifier] := Some tl;
                               let [%p QuotePat.subst_of_expr target] = hd in
                               [%e e]] in
                      attributes, k
                  end
              | _ ->
                  Location.raise_errorf ~loc:(Metapp.Attr.to_loc attribute)
                    "Unsupported binding for @for" in
        k [%meta exp]] in
  Ppxlib.Ast_helper.Exp.case pat exp

let quote_of_declaration (prefix : Longident.t) (name : string)
    (declaration : Types.type_declaration) : Ppxlib.value_binding =
  let cases =
    match declaration.type_kind with
    | Type_abstract ->
        [Ppxlib.Ast_helper.Exp.case (Metapp.Pat.var "x")
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
    | "class_type" ->
        Some [%p? { pcty_desc = Pcty_extension ({ txt = "c"; _ }, payload); _ }]
    | "class_type_field" ->
        Some [%p? { pctf_desc = Pctf_extension ({ txt = "c"; _ }, payload); _ }]
    | "class_expr" ->
        Some [%p? { pcl_desc = Pcl_extension ({ txt = "c"; _ }, payload); _ }]
    | "class_field" ->
        Some [%p? { pcf_desc = Pcf_extension ({ txt = "c"; _ }, payload); _ }]
    | _ -> None in
  let cases =
    match pat with
    | None -> cases
    | Some pat ->
        Ppxlib.Ast_helper.Exp.case pat [%e
          Target.get_mapper Mapper.mapper
            (Target.of_payload payload)] :: cases in
  let exp = [%e fun x ->
    try [%meta (Ppxlib.Ast_helper.Exp.match_ [%e x] cases)]
    with Subst { ty = [%meta Metapp.Pat.of_string name]; target } -> target] in
  let target =
    Ppxlib.Ast_helper.Typ.constr
      (Metapp.mkloc (Longident.Ldot (Lident "Target", "t"))) [] in
  let param_names =
    declaration.type_params |> List.map (fun (ty : Types.type_expr) ->
      match ty.desc with
      | Tvar (Some name) -> name
      | _ -> assert false) in
  let typ =
    Ppxlib.Ast_helper.Typ.arrow Nolabel
      (Ppxlib.Ast_helper.Typ.constr (Metapp.mkloc
        (Longident.Ldot (prefix, name)))
        (List.map Ppxlib.Ast_helper.Typ.var param_names))
      target in
  let add_param ty typ =
    Ppxlib.Ast_helper.Typ.arrow Nolabel
      (Ppxlib.Ast_helper.Typ.arrow Nolabel (Ppxlib.Ast_helper.Typ.var ty) target) typ in
  let typ = List.fold_right add_param param_names typ in
  let typ = [%t: ?subst:subst StringMap.t ->
    ?in_list:string list ref -> [%meta typ]] in
  let typ =
    match param_names with
    | [] -> typ
    | _ -> Metapp.Typ.poly (List.map Metapp.mkloc param_names) typ in
  let add_param name exp =
    Ppxlib.Ast_helper.Exp.fun_ Nolabel None (Metapp.Pat.var (quote_name name))
      exp in
  let exp = List.fold_right add_param param_names exp in
  let exp = [%e fun ?(subst = StringMap.empty) ?in_list -> [%meta exp]] in
  let pat =
    Ppxlib.Ast_helper.Pat.constraint_ (Metapp.Pat.var (quote_name name)) typ in
  Ppxlib.Ast_helper.Vb.mk pat exp

let quote_of_sig (filter : string list -> bool) (prefix : Longident.t)
    (s : Types.signature) : Ppxlib.structure_item =
  let accu_group group accu =
    match group with
    | None -> accu
    | Some (rec_flag, group) -> (rec_flag, List.rev group) :: accu in
  let add_item (group, accu) (item : Types.signature_item) =
    match Metapp.Types.Sigi.destruct_sig_type item with
    | Some { id; decl; rec_status; _ } ->
        let ((rec_status, accu_group), accu) =
          match (rec_status, group) with
          | (Trec_next, Some group) -> (group, accu)
          | (Trec_first, _) -> ((Ppxlib.Asttypes.Recursive, []), accu_group group accu)
          | (Trec_not, _) ->
              ((Ppxlib.Asttypes.Nonrecursive, []), accu_group group accu)
          | _ -> assert false in
        (Some (rec_status, (id, decl) :: accu_group), accu)
    | None -> (group, accu) in
  let (group, accu) = List.fold_left add_item (None, []) s in
  let groups = List.rev (accu_group group accu) in
  let groups = groups |> List.filter (fun (_, declarations) ->
    filter (declarations |> List.map (fun (id, _) -> Ident.name id))) in
  let s = groups |> List.map (fun (rec_flag, declarations) ->
    Ppxlib.Ast_helper.Str.value rec_flag
      (List.map (fun (id, decl) ->
        quote_of_declaration prefix (Ident.name id) decl)
        declarations)) in
  Metapp.Stri.of_list s

let () = Findlib.init ()

let compiler_libs = Findlib.package_directory "ppxlib.ast"

let signature_of_cmi filename =
  (Cmi_format.read_cmi (Filename.concat compiler_libs filename)).cmi_sign

let ppxlib_signature =
  Option.get (find_module "Ast"
  (Option.get (find_module "OCaml_412"
    (signature_of_cmi "ppxlib_ast__Versions.cmi"))))
]

module type MapperS = sig
  val mapper : mapper
end

module DefaultMap = struct
  let mapper = { expression = Fun.id; pattern = Fun.id }
end

module StringMap = Map.Make (String)

module Make (Target : QuoteValueS) = struct
  module Quoter (Mapper : MapperS) = struct
    type subst = {
        ty : string;
        target : Target.t;
      }

    exception Subst of subst

    let list_counter = ref 0

    let subst_of_value_binding (binding : Ppxlib.value_binding) :
        string * subst =
      match binding with
      | { pvb_pat = { ppat_desc = Ppat_constraint ({
            ppat_desc = Ppat_var { txt = identifier; _ }
              | Ppat_construct ({ txt = Lident identifier; _ }, None)},
            { ptyp_desc = Ptyp_constr ({ txt = Lident ty; _ }, [])
              | Ptyp_poly (_, { ptyp_desc =
                  Ptyp_constr ({ txt = Lident ty; _ }, [])})})};
          pvb_expr = expr } ->
          let expr =
            match expr with
            | { pexp_desc = Pexp_constraint (expr, _); _ } -> expr
            | _ -> expr in
          identifier, { ty; target = Target.subst_of_expr expr }
      | { pvb_loc; _ } ->
          Location.raise_errorf ~loc:pvb_loc "Typed value-binding expected"

    let parse_subst (subst : Ppxlib.attribute) : subst StringMap.t =
      match Metapp.Stri.of_payload (Metapp.Attr.payload subst) with
      | { pstr_desc = Pstr_value (Nonrecursive, values)} ->
          List.map subst_of_value_binding values |> List.to_seq |>
          StringMap.of_seq
      | { pstr_loc; _ } ->
          Location.raise_errorf ~loc:pstr_loc "Let-binding expected"

    let unit ?subst ?in_list = Target.of_unit

    let string ?subst ?in_list = Target.of_string

    let char ?subst ?in_list = Target.of_char

    let location ?subst ?in_list = Target.quote_location

    let location_stack ?subst ?in_list = Target.quote_location_stack

    let bool ?subst ?in_list = Target.of_bool

    let longident ?(subst = StringMap.empty) ?in_list (l : Longident.t) =
      try
        match
          match l with
          | Lident s -> StringMap.find_opt s subst
          | _ -> None
        with
        | None -> Target.of_longident l
        | Some subst -> raise (Subst subst)
      with Subst { ty = "longident"; target } -> target

    let list ?(subst = StringMap.empty) ?in_list (f : subst:subst StringMap.t ->
      ?in_list:string list ref -> 'a -> Target.t) (l : 'a list) : Target.t =
      try
        match Target.is_expr with
        | None -> Target.list (List.map (f ~subst ?in_list) l)
        | Some Refl ->
            let l =
              l |> List.map (fun item ->
                    let in_list = ref [] in
                    let item = f ~subst ~in_list item in
                    match !in_list with
                    | [] -> Target.list [item]
                    | lists ->
                        let item : Ppxlib.expression =
                          let loc = !Ppxlib.Ast_helper.default_loc in
                          [%expr
                          let exception End_of_list in
                          let rec loop accu =
                            match [%e item] with
                            | exception End_of_list -> List.rev accu
                            | item ->
                                loop (item :: accu) in
                          loop []] in
                        List.fold_left
                          (fun item list : Ppxlib.expression ->
                            let loc = !Ppxlib.Ast_helper.default_loc in
                            [%expr let [%p Metapp.Pat.var list] = ref None in
                            [%e item]])
                          item lists) in
            let loc = !Ppxlib.Ast_helper.default_loc in
            [%expr List.concat [%e Target.list l]]
      with Subst { ty = "list"; target } -> target

    let option ?subst ?in_list (quote_value : 'a -> Target.t)
        (option : 'a option) : Target.t =
      try
        Target.option (Option.map quote_value option)
      with Subst { ty = "option"; target } -> target

    [%%meta
       quote_of_sig (fun names -> not (List.mem "constant" names)) asttypes
         (Option.get (find_module "Asttypes" ppxlib_signature))]

    (* redefined here after constants, because we do not want substitutions on
       string constants. *)
    let string ?(subst = StringMap.empty) ?in_list (s : string) =
      try
        match StringMap.find_opt s subst with
        | None -> Target.of_string s
        | Some subst -> raise (Subst subst)
      with Subst { ty = "string"; target } -> target

    [%%meta
       quote_of_sig (fun names ->
         List.mem "constant" names || List.mem "core_type" names) ppxlib
         (Option.get (find_module "Parsetree" ppxlib_signature))]

    let quote_extension
        ((({ txt; loc }, payload), attrs) : Metapp.destruct_extension)
        : Target.t option =
      Ppxlib.Ast_helper.with_default_loc loc @@ fun () ->
      let subst =
        Metapp.Attr.find "subst" attrs |> Option.map parse_subst in
      let prefix = "metaquot." in
      let txt =
        if
          try String.sub txt 0 (String.length prefix) = prefix
          with Invalid_argument _ -> false
        then
          String.sub txt (String.length prefix)
            (String.length txt - String.length prefix)
        else
          txt in
      match txt with
      | "expr" ->
          Some (expression ?subst (Metapp.Exp.of_payload payload))
      | "pat" ->
          Some (pattern ?subst (Metapp.Pat.of_payload payload))
      | "str" ->
          Some (structure ?subst (Metapp.Str.of_payload payload))
      | "stri" ->
          Some (structure_item ?subst (Metapp.Stri.of_payload payload))
      | "sig" ->
          Some (signature ?subst (Metapp.Sig.of_payload payload))
      | "sigi" ->
          Some (signature_item ?subst (Metapp.Sigi.of_payload payload))
      | "type" ->
          Some (core_type ?subst (Metapp.Typ.of_payload payload))
      | "lid" ->
          Some (longident ?subst (Metapp.Longident.of_payload payload))
      | _ -> None
  end

  let lift (super : mapper) (e : Target.t) : Target.t =
    let module Mapper = struct
      let mapper = super
    end in
    let module Quoter = Quoter (Mapper) in
    Ppxlib.Ast_helper.with_default_loc (Target.to_loc e) @@ fun () ->
    match Option.bind (Target.destruct_extension e) Quoter.quote_extension with
    | Some e -> e
    | None -> Target.get_mapper super e

  include Quoter (DefaultMap)
end

module Exp = Make (QuoteExp)

module Pat = Make (QuotePat)
