let available = false

let run (_ : Yojson.Safe.t) : string * bool =
  ( "abella2tex support was not compiled in: the abella2tex opam package \
     was not installed when abella_mcp was built. Install abella2tex and \
     rebuild to enable this tool.",
    true )
