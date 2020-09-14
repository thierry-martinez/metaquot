let mapper = object (self)
  inherit Ppxlib.Ast_traverse.map as super

  method! expression =
    Metaquot.Exp.lift { expression = super#expression; pattern = super#pattern }

  method! pattern =
    Metaquot.Pat.lift { expression = super#expression; pattern = super#pattern }
end

let () =
  Ppxlib.Driver.register_transformation "metaquot" ~impl:mapper#structure
