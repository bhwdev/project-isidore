# bhw-www

## Architecture

One SBCL process serves four parts, each a group of package-inferred
systems under `src/`:

- `site/` -- the personal website (`pages`) and the frame every page shares
  (`layout`).
- `bible/` -- the Douay-Rheims reader: its graph-db store (`database`), text,
  links and Montezuma search (`text`), pages (`reader`).
- `slack-bot/` -- the van-booking bot: its graph-db store (`database`),
  `nlp` / `booking-nlp`, the schedule spreadsheet
  (`van-booking`, through the Apps Script in `van-booking.gs`), and the Slack
  endpoints and reminder scheduler (`bot`).
- `plp/` -- the praise lyrics presenter: a song as data, its text format
  (chords written in the words as `[G]`), its slides and the transposing of
  its chords (`song`), its graph-db store of songs, tags and setlists
  (`database`), the chord book's import (`import`), the rooms that tie a
  console to its screens (`live`), and the pages `/plp`, `/plp-console/CODE`
  (`console`, which is also the musician's page: each slide once, chords
  over the words, sized by the server) and `/plp-screen/CODE` (`screen`). Its pages are public; saving
  a song or a setlist needs `$BHW_PLP_PASSPHRASE`. The lyrics are not in the
  repository: store, staging files and waiting images are under
  `<data root>/plp/`, and the tests use a public-domain hymn. `import` runs
  poppler's `pdftotext`, on a development machine only:
  `(bhw/plp/import:import-book PDF)` stages the songs with a `REPORT.txt`,
  `(bhw/plp/import:load-staging)` stores them, as often as they are corrected.
  Chords are placed by the PDF's word coordinates (`pdftotext -tsv`);
  `add-chords-to-staging` and `add-chords-to-store` give them to songs staged
  and stored without, leaving their corrections.
  The one script of the site's own is the expression that asks a browser its
  clock's offset from UTC (`screen:timezone-offset`).

`main.lisp` is the composition root: `initialize-application` opens the
stores, starts the scheduler and CLOG, and has each part register its routes.
Loading the system opens and starts nothing. The executable turns off
CLOG's `*break-on-error*`, as an error in a page's thread would otherwise
exit the process; there is no static root, so only `assets/` (and the
favicon) of the checkout is served. The web server is `site-acceptor`
there, a Hunchentoot acceptor subclassing the CLOG fork's `clog-acceptor`: it
answers the request guard, the Slack endpoints, POST /theme and the
presenter's `/plp-images/` before CLOG,
and sends files with ETags (its `serve-static-file` method). Those handlers
take a request plist and return `(status headers body)`; `request-env` and
`reply` convert, so they are tested without a server. `store.lisp` is the
graph-db plumbing the stores share (one system directory per process). Pages
run no script of the site's own (CLOG's own runtime aside): the colour theme
is a cookie set by POST /theme (`site/layout`), and the index's slideshow and
greeting are CSS.

`clog/` is a fork of CLOG (system `clog`, `clog.asd` at the root beside
`bhw.asd`): its core and CLOG-GUI, without MGL-PAX, CL-DBI,
Clack, Lack, websocket-driver or jQuery, on Hunchentoot with its own RFC 6455
websockets (`clog-websocket.lisp`); the scripts it sends the browser are plain
DOM calls and the helpers in `clog/static-files/js/boot.js`.
`clog/README.org` lists what was cut and changed from upstream's commit; its
tests are `src/tests/clog.lisp`.

Packages `:use` only `common-lisp`; everything else comes in through
`:import-from` or `:local-nicknames`. Tests are Rove, under `src/tests/`
mirroring `src/`.

## Commands

- `guix build -f guix.scm`: compiles, runs every test suite (the `check`
  phase), then saves the executable (`bin/bhw-www` in its store
  output) from an image that loaded only the system. Its source is the files
  of HEAD's commit (Guix's `git-predicate`) with their working-tree contents,
  so a new file is left out until it is committed. Production builds the same
  file with the host's Guix (README.org, "Deployment").
- `guix shell -D -f guix.scm`: the development environment. Its cache follows
  `guix.scm`'s mtime, so it is never stale.
- `guix.scm` grafts a CFFI without GCC (`with-cffi/no-gcc`): `--no-grafts`
  brings GCC back. Only a variant with every file of the original can be
  grafted: ASDF configurations link into each other's, and a graft leaves
  links to missing files dangling. The CLOG fork's dependencies are the
  package's own inputs.

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
  `bhw/tests/<path>` (e.g. `bhw/tests/slack-bot/nlp`)
  gives per-test results. When a failure is not inside a `testing` block it
  dies with "The function NIL is undefined"; then use `repl-eval`
  `(rove:run :bhw/tests/<path> :style :dot)` for the report.

Use the built-in tools for:
- Non-Lisp files, small files, and edits of a few lines (`Edit` costs what
  `lisp-patch-form` does).
- `src/tests/slack-bot/bot.lisp`: cl-mcp's editors cannot parse it (SBCL
  character names). Edit it with `Edit`; its suite runs in the worker.
- The full gate: `guix build -f guix.scm`, through Bash with the output
  filtered.

Setup: a local-scope MCP server in `~/.claude.json` runs `cl-mcp-stdio` (from
Guix Home) inside `guix shell -D -f /home/ben/bhw-www/guix.scm`, so
the worker finds the dependencies however Claude Code was started. Then
`load-system` `bhw/tests`. After a change to `guix.scm` that
needs a package built, build the shell once (`guix shell -D -f guix.scm --
true`) before starting Claude Code, or cl-mcp may time out connecting. Not
under another Guix's `time-machine`: its SBCL may differ from cl-mcp's, and
loading then fails with `INVALID-FASL-VERSION`.
