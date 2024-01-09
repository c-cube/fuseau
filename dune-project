(lang dune 2.0)

(name fuseau)

(generate_opam_files true)

(source
 (github c-cube/fuseau))

(authors "Simon Cruanes")

(maintainers "Simon Cruanes")

(license MIT)

;(documentation https://url/to/documentation)

(package
 (name fuseau)
 (synopsis "An IO and concurrency library built on picos+libuv")
 (depends
  (ocaml
   (>= 5.0))
  dune
  luv
  picos
  thread-local-storage
  (cstruct
   (>= 6.0))
  atomic)
 (tags
  (libuv await effects event-loop thread-local-storage)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
