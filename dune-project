(lang dune 3.10)

(name snake)

(generate_opam_files true)

(source
 (github stswn/snake))

(authors "Stanisław Swianiewicz")


(package
 (name snake)
 (synopsis "Snake game")
 (description "Toy project implementing game of Snake using Raylib")
 (depends ocaml dune raylib ppx_deriving ppx_inline_tests)
 (tags
  (topics raylib snake game)))

