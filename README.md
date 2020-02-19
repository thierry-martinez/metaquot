# `metaquot`: OCaml syntax extension for quoting code

`metaquot` is a PPX rewriter that provides several extensions for
quoting OCaml code in expressions, _à la_ [`ppxtools.ppx_metaquot`]
and [`ppxlib.metaquot`].  In comparison to these libraries, `metaquot`
is built by meta-programmation over the [`Parsetree`] module (thanks
to [`metapp`]) and is meant to be trivial to update for future
versions of OCaml (on the other hand, `metaquot` only builds AST for
the current version of OCaml: you may use helpers provided by
[`Ast_helper`] or [`Metapp`] to manipulate the AST in a
version-independent manner).

[`ppxtools.ppx_metaquot`]: https://github.com/ocaml-ppx/ppx_tools
[`ppxlib.metaquot`]: https://github.com/ocaml-ppx/ppxlib
[`Parsetree`]: https://caml.inria.fr/pub/docs/manual-ocaml/compilerlibref/Parsetree.html
[`metapp`]: https://github.com/thierry-martinez/metapp
[`Ast_helper`]: https://caml.inria.fr/pub/docs/manual-ocaml/compilerlibref/Ast_helper.html
[`Metapp`]: https://github.com/thierry-martinez/metapp/blob/master/metapp/metapp.mli

`metaquot` can be used with [`dune`] by using the [`preprocess`] field.

[`dune`]: https://github.com/ocaml/dune
[`preprocess`]: https://dune.readthedocs.io/en/latest/concepts.html#preprocessing-with-ppx-rewriters

```lisp
(executable
  ...
  (preprocess (pps metaquot.ppx))
  ...)
```

The following extensions are provided.

|Quotation     |Type                      |
|--------------|--------------------------|
|`[%expr ...]` |`Parsetree.expression`    |
|`[%pat? ...]` |`Parsetree.pattern`       |
|`[%type: ...]`|`Parsetree.core_type`     |
|`[%sig: ...]` |`Parsetree.signature`     |
|`[%sigi: ...]`|`Parsetree.signature_item`|
|`[%str ...]`  |`Parsetree.structure`     |
|`[%stri ...]` |`Parsetree.structure_item`|

The produced AST uses `!Ast_helper.default_loc` as location: you may
change the location with `Ast_helper.with_default_loc`.

Moreover, in quoted code, the following extensions allow to evaluate
and insert expressions or patterns (_anti-quotations_).

|Anti-quotation|Type                                                    |
|--------------|--------------------------------------------------------|
|`[%e ...]`    |`Parsetree.expression`                                  |
|`[%p ...]`    |`Parsetree.pattern`                                     |
|`[%t ...]`    |`Parsetree.core_type`                                   |
|`[%m ...]`    |`Parsetree.module_type` or `Parsetree.module_expr`      |
|`[%i ...]`    |`Parsetree.signature_item` or `Parsetree.structure_item`|

In addition to the syntax extension, the `metaquot` package provides
the [`Metaquot`] module, which contains lifters: the `Metaquot.Exp`
module lifts to expressions and `Metaquot.Pat` lifts to patterns. For
instance, `Metaquot.Exp.pattern : Parsetree.pattern ->
Parsetree.expression` is a function that returns an OCaml expression
that builds the AST corresponding to the given pattern.

[`Metaquot`]: https://github.com/thierry-martinez/metaquot/blob/master/metaquot/metaquot.ml
