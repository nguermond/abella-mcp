# abella-mcp

**`abella_mcp`** (`bin/abella_mcp.ml`) — an MCP server exposing the
[Abella](https://abella-prover.org/) proof assistant to an agent. The agent
drives Abella interactively over a long-lived session instead of shelling out
and re-running whole files, and — via the (separate) `abella2tex` library — can
render Abella terms and statements to TeX without a separate CLI either.

## Tools

The server exposes seven tools over MCP stdio.

| Tool | Purpose |
| --- | --- |
| `abella_check` | Batch-check a `.thm` file; reports the diagnostic and the lines around it, and how many proofs were closed with `skip`. |
| `abella_start` | Start or restart an interactive session, optionally loading a file. |
| `abella_send` | Send commands; returns the transcript and what each command *changed* in the proof state. |
| `abella_state` | Show the current proof state in full, without changing it. |
| `abella_undo` | Undo the last *n* proof commands. |
| `abella_stop` | Stop the session and release the Abella subprocess. |
| `abella2tex` | Render a term, `Define` clauses, or a whole `.thm` file's declarations to TeX under a notation configuration. Note: `abella2tex` must be installed separately. |

### Picking up work in progress

`abella_start` on a file whose last proof is **incomplete** loads everything
and parks the session exactly at the open subgoal — ready to continue, which is
how work in progress gets picked up. Pass the file as the tool's arguments:

```json
{ "file": "terms.thm" }
```

### Reporting the change, not the state

After every tactic Abella reprints the entire proof state: the induction
hypothesis, every untouched hypothesis, the goal, and the statement of each
pending subgoal. Over a long proof that is the same text again and again, and
for an agent paying by the token it is the bulk of the transcript. So
`abella_send` reports only what a command actually changed:

```
> case H2.
- H2
H5 : mem (of T Tau) Ctx
(unchanged: IH, H3, H4)
goal: (unchanged)
```

New and altered hypotheses appear in full — those are the ones being reasoned
about; removed ones are named on the `-` line; untouched ones only named.
`goal:` carries the conclusion when it moves and `(unchanged)` when it does
not, a `Subgoal N:` header appears when the position changes, and pending
subgoals are reduced to a count. A tactic that changes nothing — a failed
`search`, say — collapses to `(state unchanged)` under the error.

Nothing is lost: `abella_state` prints the full state, pending subgoals and
all, and `abella_send` accepts `verbose: true` to get Abella's raw output for
every command. The diff is computed on collapsed whitespace, so a formula that
Abella merely re-wraps (its layout depends on the width of the `H1 : ` prefix)
never reads as a change.


### Skips

`skip` closes a goal without proving it. Abella marks such a proof `Proof
completed *** USING skip ***` and still exits 0, so a file whose proofs are all
skipped passes an unqualified check. `abella_check` therefore reports them,
attributed to the theorems they close, in file order:

```
test.thm: OK -- 30 proof(s) completed.

8 proof(s) closed with skip: lemma1, lemma2, ...
```

## Build

```sh
dune build
```

The MCP server binary lands at `_build/default/bin/abella_mcp.exe`.

`abella_mcp`'s dependencies are `unix`, `yojson`,
[`jsonrpc`](https://opam.ocaml.org/packages/jsonrpc/) (the JSON-RPC 2.0 message
layer, shared with `ocaml-lsp`), and, optionally, `abella2tex`.

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

`ABELLA_BIN` selects the Abella binary; without it the server takes `abella`
from `PATH`, falling back to `~/.local/bin/abella`.
