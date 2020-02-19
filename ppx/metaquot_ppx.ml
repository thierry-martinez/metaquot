let mapper : Ast_mapper.mapper = { Ast_mapper.default_mapper with
  expr = Metaquot.Exp.lift;
  pat = Metaquot.Pat.lift;
}

let rewriter _config _cookies : Ast_mapper.mapper =
  mapper

let () =
  Migrate_parsetree.Driver.register ~name:"metaquot"
    (module Migrate_parsetree.OCaml_current)
    rewriter
