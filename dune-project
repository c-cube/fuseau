(lang dune 2.0)

(name luv2loop)
(generate_opam_files true)

(source
 (github c-cube/luv2loop))

(authors "Simon Cruanes")
(maintainers "Simon Cruanes")
(license MIT)

;(documentation https://url/to/documentation)

(package
 (name luv2loop)
 (synopsis "An IO and concurrency library built on libuv")
 (depends (ocaml (>= 5.0)) dune luv picos atomic)
 (tags
  (libuv await effects event-loop thread-local-storage)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
