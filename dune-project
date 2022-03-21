(lang dune 1.11)

(generate_opam_files true)

(name metaquot)
(license "BSD-2-Clause")
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (uri "git+https://github.com/thierry-martinez/metaquot.git"))
(homepage "https://github.com/thierry-martinez/metaquot")
(bug_reports "https://github.com/thierry-martinez/metaquot")
(documentation "https://github.com/thierry-martinez/metaquot")
(version "0.5.1")

(package
 (name metaquot)
 (synopsis "OCaml syntax extension for quoting code")
 (description "\
metaquot allows to quote OCaml code.
")
 (depends
  (ocaml (>= 4.08.0))
  (stdcompat (>= 12))
  (ppxlib (>= 0.22.0))
  (ocamlfind (>= 1.8.1))
  (dune (>= 1.11.0))
  (metapp (>= 0.4.1))
  (odoc (and :with-doc (>= 1.5.1)))))
