(lang dune 2.9)
(using mdx 0.1)

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
 (synopsis "A simple IO and concurrency library for OCaml 5")
 (depends
  (ocaml
   (>= 5.0))
  dune
  (mtime
   (>= 2.0))
  trace
  base-unix
  (trace-tef :with-test)
  thread-local-storage)
 (tags
  (await effects event-loop thread-local-storage)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
