(lang dune 3.7)
(name abella_mcp)
(generate_opam_files true)

(package
 (name abella_mcp)
 (synopsis "MCP server exposing the Abella proof assistant, including abella2tex rendering")
 (depends ocaml yojson jsonrpc abella2tex))
