(** Shared text-handling helpers for the Abella REPL driver.

    These were carved out of [abella_mcp.ml] so both the server and the
    proof-state summariser can use them without depending on each other. *)

val trim : string -> string
(** [String.trim] wrapped for call-site brevity. *)

val contains : needle:string -> string -> bool
(** Whether [needle] occurs in the haystack. *)

val lines_of : string -> string list
(** Split on newlines. *)

val is_name_char : char -> bool
(** Whether a character may appear in an Abella name (identifiers, and the
    ['], [-], [*] suffixes that show up in printed hypotheses). *)
