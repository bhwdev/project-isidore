# Project Isidore

## Architecture

One SBCL process serves three parts, each a group of package-inferred
systems under `src/`:

- `site/` -- the personal website (`pages`) and the frame every page shares
  (`layout`).
- `bible/` -- the Douay-Rheims reader: its graph-db store (`database`), text,
  links and Montezuma search (`text`), pages (`reader`).
- `slack-bot/` -- the van-booking bot: its graph-db store (`database`),
  `nlp` / `booking-nlp`, the schedule spreadsheet
  (`van-booking`, through the Apps Script in `van-booking.gs`), and the Slack
  endpoints and reminder scheduler (`bot`).

`main.lisp` is the composition root: `initialize-application` opens the
stores, starts the scheduler and CLOG, and has each part register its routes.
Loading the system opens and starts nothing. `store.lisp` is the graph-db
plumbing the stores share (one system directory per process).

Packages `:use` only `common-lisp`; everything else comes in through
`:import-from` or `:local-nicknames`. Tests are Rove, under `src/tests/`
mirroring `src/`.

## Commands

- `guix build -f guix.scm`: compiles, runs every test suite (the `check`
  phase), then saves `bin/project-isidore` from an image that loaded only the
  system. Its source is the files of HEAD's commit (Guix's `git-predicate`)
  with their working-tree contents, so a new file is left out until it is
  committed.
- `guix shell -D -f guix.scm`: the development environment. Its cache follows
  `guix.scm`'s mtime, so it is never stale.

## cl-mcp

cl-mcp gives a persistent SBCL worker with the project loaded. Use it where it
costs fewer tokens than the built-in tools, and the built-in tools everywhere
else: it wins when the alternative is reading a whole file or starting an SBCL,
and loses on small files, non-Lisp files and short edits.

Use cl-mcp for:
- **Questions about running code**: `repl-eval` in the worker, not a standalone
  `sbcl --load` script, which reloads the system on every question (minutes)
  and leaves a log to filter. Likewise `code-find-references`, `clos-describe`,
  `lisp-macroexpand`, `inspect-object`.
- **One definition in a large file**: `lisp-read-file` with `name_pattern`, or
  `collapsed=true` for an outline, rather than reading `slack-bot/bot.lisp` or
  `slack-bot/van-booking.lisp` whole.
- **Finding a definition or its callers**: `clgrep-search` returns the
  enclosing form's signature, so a hit needs no second read for context.
- **One test package**: `run-tests` with system
  `project-isidore/tests/<path>` (e.g. `project-isidore/tests/slack-bot/nlp`)
  gives per-test results. When a failure is not inside a `testing` block it
  dies with "The function NIL is undefined"; then use `repl-eval`
  `(rove:run :project-isidore/tests/<path> :style :dot)` for the report.

Use the built-in tools for:
- Non-Lisp files, small files, and edits of a few lines (`Edit` costs what
  `lisp-patch-form` does).
- `src/tests/slack-bot/bot.lisp`: cl-mcp's editors cannot parse it (SBCL
  character names). Edit it with `Edit`; its suite runs in the worker.
- The full gate: `guix build -f guix.scm`, through Bash with the output
  filtered.

Setup: a local-scope MCP server in `~/.claude.json` runs `cl-mcp-stdio` (from
Guix Home) inside `guix shell -D -f /home/ben/project-isidore/guix.scm`, so
the worker finds the dependencies however Claude Code was started. Then
`load-system` `project-isidore/tests`. After a change to `guix.scm` that
needs a package built, build the shell once (`guix shell -D -f guix.scm --
true`) before starting Claude Code, or cl-mcp may time out connecting. Not
CI's Guix (`guix time-machine --commit=v1.5.0`): its fasls are SBCL 2.5.8,
the worker is cl-mcp's 2.6.4, and loading fails with `INVALID-FASL-VERSION`.
