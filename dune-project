(lang dune 3.16)
(using menhir 3.0)
(name FaustML)

(generate_opam_files true)

(source
 (github TravisLEBLANC1/FaustML))

(authors "Travis LEBLANC")

(maintainers "Travis LEBLANC")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name FaustML)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
