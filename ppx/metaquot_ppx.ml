let mapper = object (self)
  inherit Ppxlib.Ast_traverse.map as super

  method! expression exp =
    Gc.minor (); (* This seems to solve a segfault in OCaml 5.0
      but this is weird!*)
    Metaquot.Exp.lift { expression = super#expression; pattern = super#pattern }
      exp

  method! pattern =
    Metaquot.Pat.lift { expression = super#expression; pattern = super#pattern }
end

let () =
  Ppxlib.Driver.register_transformation "metaquot" ~impl:mapper#structure
