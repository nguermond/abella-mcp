(** Rendering Abella terms and statements to TeX, via the abella2tex library.

    abella2tex is an external opam dependency (its own project), and
    abella_mcp should still build without it. [dune]'s [select] picks the
    real implementation when abella2tex is installed and a stub otherwise,
    so callers just see [available] go false and [run] explain why. *)

val available : bool
(** Whether abella2tex was present at build time. *)

val run : Yojson.Safe.t -> string * bool
(** Run the abella2tex tool on the MCP call's arguments, returning
    (text, is_error). *)
