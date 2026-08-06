(lang dune 3.7)
(name abella_mcp)
(generate_opam_files true)
(source (github nguermond/abella-mcp))
(license GPL-3.0-or-later)
(authors "Nathan Guermond")
(maintainers "nguermond@proton.me")

 
(package
 (name abella_mcp)
 (version 0.2.0)
 (synopsis "MCP server exposing the Abella proof assistant, including abella2tex rendering")
 (depends ocaml yojson jsonrpc)
 (depopts abella2tex))
