(lang dune 3.16)
(using menhir 3.0)
(name FaustML)

(generate_opam_files true)

(source
 (github TravisLEBLANC1/FaustML))

(authors "Travis LEBLANC")

(maintainers "Travis LEBLANC")

(license LICENSE)


(package
 (name FaustML)
 (synopsis "First order simply typed ML, proved to be sound for polytime")
 (description "if a program satisfy data-tiering/typing/syntax restrictions;
 it will execute in polytime using memoization and sharing")
 (depends ocaml dune)
 (tags
  ("ICC" "Implicit Computational Complexity" "polytime" )))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
