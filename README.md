# abella-mcp

This project exposes the [Abella](https://abella-prover.org/) proof assistant 
to an agent via an MCP server. The server exposes several tools allowing for
simultaneous interactive sessions. The proof state is rendered as a diff
for more efficient token use. An abella to TeX tool (`abella2tex`) is also available
via the server, but must be installed separately.

## Tools

The server exposes eight tools:

| Tool | Purpose |
| --- | --- |
| `abella_check` | Batch-check a `.thm` file; reports the diagnostic and the lines around it, and how many proofs were closed with `skip`. |
| `abella_start` | Start or restart an interactive session, optionally loading a file. |
| `abella_send` | Send commands; returns the transcript and what *changed* in the proof state. |
| `abella_state` | Show the current proof state in full, without changing it. |
| `abella_undo` | Undo the last *n* proof commands. |
| `abella_stop` | Stop the session and release the Abella subprocess. |
| `abella_sessions` | List all currently running sessions. |
| `abella2tex` | Render a term, `Define` clauses, or a whole `.thm` file's declarations to TeX under a notation configuration. Note: `abella2tex` must be installed separately. |


## Build

Use `dune build` or `opam install .` to install.

Dependences are the opam packages `yojson` and `jsonrpc`.

## Configuration

Point an MCP client's server config at the `abella_mcp` executable, 
as well as the `abella` executable, e.g. in a project's `.mcp.json`:

```json
{
  "mcpServers": {
    "abella": {
      "command": "/path/to/abella-mcp/_build/default/bin/abella_mcp.exe",
      "args": [],
      "env": { "ABELLA_BIN": "/path/to/abella" }
    }
  }
}
```

If `ABELLA_BIN` is not specified, `abella` must be specified in the `PATH`.
