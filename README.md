# abella-mcp

**`abella_mcp`** (`bin/abella_mcp.ml`) — an MCP server exposing the
[Abella](https://abella-prover.org/) proof assistant, so that an agent
can drive Abella interactively instead of shelling out and re-running
whole files. It also links the `abella2tex` library, exposing TeX
rendering of Abella terms and statements as an MCP tool (below), so an
agent never has to shell out to a separate CLI for that either.

`abella2tex` is a separate project (its own opam package, pinned into
this switch); see its own README for the library and its notation
configuration format.

## Build

```sh
dune build
```

The MCP server binary lands at `_build/default/bin/abella_mcp.exe`.

`abella_mcp`'s dependencies are `unix`, `yojson`,
[`jsonrpc`](https://opam.ocaml.org/packages/jsonrpc/) (the JSON-RPC 2.0 message
layer, shared with `ocaml-lsp`), and `abella2tex`; `unix` and `yojson` already
ship with Abella, `jsonrpc` is a one-line `opam install jsonrpc`, and
`abella2tex` needs to be pinned or installed from its own repository before
this one builds.

The module interface is `bin/abella_mcp.mli`: it exposes just `main` (run the
server loop) and `protocol_version`, keeping the session handling, tool
registry, and Abella REPL plumbing private.

## Configuration

Point an MCP client's server config at `_build/default/bin/abella_mcp.exe`,
e.g. in a consuming project's `.mcp.json`:

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

`ABELLA_BIN` selects the Abella binary; without it the server takes
`abella` from `PATH`, falling back to `~/.local/bin/abella`.

## Tools

| Tool | Purpose |
| --- | --- |
| `abella_check` | Batch-check a `.thm` file; reports the diagnostic and the lines around it, and how many proofs were closed with `skip`. |
| `abella_start` | Start/restart an interactive session, optionally loading a file. |
| `abella_send` | Send commands; returns the transcript and what each command *changed* in the proof state. |
| `abella_state` | Show the current proof state in full without changing it. |
| `abella_undo` | Undo the last *n* proof commands. |
| `abella_stop` | Stop the session. |
| `abella2tex` | Render a term, `Define` clauses, or a whole `.thm` file's declarations to TeX under a notation configuration. Reports variable-naming collisions inline. |

The useful trick: `abella_start` on a file whose last proof is **incomplete**
loads everything and parks the session exactly at the open subgoal, ready to
continue — which is how work in progress gets picked up.

### Reporting the change, not the state

Abella reprints the entire proof state after every tactic: the induction
hypothesis, every untouched hypothesis, the goal, and the statement of each
pending subgoal. Over a long proof that is the same text again and again, and
for an agent paying by the token it is the bulk of the transcript. So
`abella_send` reports only what a command actually changed:

```
> case H2.
- H2
H5 : smem (dconst T Tau) Sig1
(unchanged: IH, H3, H4)
goal: (unchanged)
```

New and altered hypotheses appear in full — those are the ones being reasoned
about; removed ones are named on the `-` line, untouched ones only named.
`goal:` carries the conclusion when it moves and `(unchanged)` when it does
not, a `Subgoal N:` header appears when the position changes, and the pending
subgoals are reduced to a count. A tactic that changes nothing — a failed
`search`, say — collapses to `(state unchanged)` under the error.

On a real 15-tactic proof from `terms.thm` this halves the transcript, and the
saving grows with the size of the state, since what is elided is exactly what
is being repeated.

Nothing is lost: `abella_state` prints the full state, pending subgoals and
all, and `abella_send` takes `verbose: true` to get Abella's raw output for
every command. The diff is computed on collapsed whitespace, so a formula that
Abella merely re-wraps (its layout depends on the width of the `H1 : ` prefix)
never reads as a change.

### Skips

`skip` closes a goal without proving it. Abella marks such a proof `Proof
completed *** USING skip ***` and still exits 0, so a file whose proofs are
all skipped passes an unqualified check. `abella_check` therefore reports them,
attributed to the theorems they close, in file order:

```
grounding.thm: OK -- 30 proof(s) completed.

8 proof(s) closed with skip: sapp_sharp, sapp_apply, sapp_t_open, ...
```

## How it works

Abella is driven as a subprocess over its interactive REPL (`abella file.thm -i`),
the same surface Abella's own Emacs mode uses. The protocol is strict
request/response: write `<command>\n`, then read until the output ends with the
prompt Abella writes as `<name> < ` (`Abella` at top level, else the theorem's
name).

Plain pipes are enough — no PTY. Abella flushes both its output and its prompt,
so a pipe stays in sync. (A pipe *appears* to lag by one command if the reader
mixes `select` on the file descriptor with a buffered text reader that has
already drained it — that is a bug in the reader, not in Abella.)

Errors are detected by inspecting the output text, since Abella reports them on
an otherwise ordinary reply rather than through an exit status. `abella_send`
stops at the first failing command, so the effects of the commands before it
still stand and the session stays usable.

## Possible next step

The server currently parses Abella's textual output. `abella_lib` is a real
wrapped dune library, and `Prover.state_json ()` already produces the proof state
(goal, hypotheses, variables, subgoal count) as JSON. Linking it directly would
remove the text parsing entirely. The cost is that the command dispatch lives in
`abella.ml`, which is excluded from the library, so it would have to be
duplicated — hence the subprocess approach for now, which works against any
stock Abella.
