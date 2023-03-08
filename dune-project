(lang dune 3.7)

(name oshell2)

(generate_opam_files true)

(source
 (github lmouelle/oshell2))

(authors "Lucas Ouellette-Falkenstein")

(using menhir 2.0)

(package
 (name oshell2)
 (synopsis "Shell in OCaml, second try by me")
 (depends ocaml dune ounit2 menhir))
