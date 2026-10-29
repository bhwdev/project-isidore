# Project Isidore

## Architecture

## Commands

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
  `collapsed=true` for an outline, rather than reading `webserver.lisp`,
  `slack-bot.lisp` or `van-booking.lisp` whole.
- **Finding a definition or its callers**: `clgrep-search` returns the
  enclosing form's signature, so a hit needs no second read for context.
- **One test package**: `repl-eval`
  `(parachute:test :project-isidore/tests/<pkg> :report 'parachute:quiet)`.
  `run-tests` has no Parachute support; it falls back to ASDF's test-op, which
  runs every suite and returns only text.

Use the built-in tools for:
- Non-Lisp files, small files, and edits of a few lines (`Edit` costs what
  `lisp-patch-form` does).
- `src/tests/slack-bot-tests.lisp`: cl-mcp's editors cannot parse it (SBCL
  character names) and its suite has crashed the worker. Edit it with `Edit`
  and run it in a standalone SBCL.
- Full gates: `guix build -f guix.scm`, or `make.lisp`, through Bash with the
  output filtered.

Setup: the server is `cl-mcp-stdio` from Guix Home (user-level MCP config).
Start Claude Code from `guix shell -m manifest.scm`, then `load-system`
`project-isidore/tests`. After a change to `guix.scm`, add `--rebuild-cache`:
the shell's cache is keyed on `manifest.scm` alone, and a stale one loads the
old dependencies. Not from `guix time-machine -C channels.scm -- shell`: its
fasls are SBCL 2.5.8, the worker is cl-mcp's 2.6.4, and loading fails with
`INVALID-FASL-VERSION`. Loading opens the CRUD store under
`$PROJECT_ISIDORE_DATA`, else `~/.local/share/project-isidore/`.
