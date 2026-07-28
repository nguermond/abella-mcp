(** An MCP server exposing the Abella proof assistant over stdio.

    The transport is newline-delimited JSON-RPC 2.0 --- built on the
    [jsonrpc] package for message parsing and serialization --- carried
    on stdin/stdout, with all logging on stderr.  Abella itself is
    driven as a subprocess through its interactive REPL. *)

val protocol_version : string
(** The MCP protocol version advertised by the server during
    [initialize]. *)

val main : unit -> unit
(** Run the server loop: read JSON-RPC packets from stdin, answer tool
    requests (and ignore notifications) by driving Abella, and write
    responses to stdout until end of input.  Called automatically when
    the executable starts. *)
