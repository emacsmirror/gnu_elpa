# Gnosis

Gnosis combines Org-backed notes (nodes) with SQLite-backed questions
(themata), scheduled review, and non-rescheduling practice. Read the relevant
module and its callers before changing a contract; the manual is
`docs/gnosis.org`, and `Makefile` owns the development commands.

## Architecture

Sources live in `lisp/`, behavioral ERT tests in `tests/`. These are ownership
rules for changes, not a claim that every existing caller already follows them.
Read the affected implementation and callers; extract mixed responsibilities
only where the task needs it, rather than reorganizing the package:

- `gnosis-fsrs`: pure memory-model calculations. Supply state, elapsed time,
  outcome, and retention explicitly; return values without storage or UI.
- `gnosis-sqlite`: Lisp/SQL encoding, query compilation, connections, and
  transactions. Keep application schema and presentation out of this layer.
- `gnosis-db`: validated database opening, schema, migrations, and shared
  queries. Publish a connection only after successful initialization.
- `gnosis-scheduler`: review validation, acceptance, and replay. Calculate
  evidence before committing the event and its scheduler projection together.
  `gnosis-logical-day` owns logical-calendar and day-cutoff rules.
- `gnosis-org`, `gnosis-nodes`, `gnosis-journal`, and `gnosis-links`: native Org
  interpretation, file operations, and indexes. Org files own node contents;
  indexed titles and links do not replace those files or their IDs.
- `gnosis-study`: topic selection, repair, and study evidence queries/views.
  `gnosis-review` owns encounter/session flow, durable practice sessions,
  and practice success/retry policy. `gnosis-agent` adapts these operations
  without duplicating study policy.
- `gnosis-dashboard`: views of application state. Formatting returns display
  values; renderers own buffer changes and pending work. `gnosis-tl` supplies
  generic tabulated-list rendering, not application-state ownership.
- `gnosis-tags` owns tag-set operations; `gnosis-cloze` owns cloze transforms.
  Keep their data transformations separate from prompts and Org rendering.
- `gnosis-anki` and `gnosis-export-import`: parse and normalize input, derive
  changes, then apply them at explicit transaction/file boundaries.
- `gnosis-vc`: Git processes and their completion lifecycle. `gnosis.el`
  retains shared commands and authoring entry points, not new low-level policy.

Dependencies point toward the responsible data/computation layer. Keep lower
layers independent of commands and views. Reuse an existing domain operation
before adding a second path that writes the same state.

## Functional design

- Prefer explicit inputs, bound intermediate values, and returned plain data.
  A reader should be able to follow a calculation without tracing globals.
- Keep clocks, randomness, database access, prompts, and buffer/file/process
  effects in named boundary functions. Compute a result before applying it.
- Use ordinary lists, alists, and plists with documented shapes. Keep row
  decoding and representation-dependent operations beside their owner rather
  than spreading positional indexing through unrelated callers.
- Treat caller-owned inputs and retained snapshots as values. Destructive
  operations are appropriate for privately owned builders and Emacs state;
  make that ownership clear. Preserve previous inputs in transformation tests.
- Extract an abstraction when it names a real invariant or removes recurring
  structure. Prefer a direct function to a one-use wrapper, macro, registry,
  or generic framework. Smaller functions must make the data flow clearer.
- Use bounded iteration where needed; Emacs Lisp has no tail-call guarantee.
  Measure before adding caches or optimizing a pure pipeline into a fused loop.

## State and preservation

- SQLite owns scheduling and study history. Scheduled acceptance is atomic;
  duplicate event identity must agree with retained facts. Practice never
  writes FSRS grades, scheduler state, or scheduled-review replay.
- Keep identities through actions: an Org ID is not its title, and a displayed
  summary is not whichever session happens to be current. Bind actions and
  asynchronous work to their database, session, buffer, or generation owner.
- A view is a projection, not a second authority. Refresh must observe relevant
  mutations, and pending rendering must not resurrect removed or stale rows.
- Persistent Lisp encoding must round-trip independently of display-oriented
  printer settings. Preserve meaningful distinctions between nil and empty text.
- Validate both the reviewed input and the destination state before applying
  an import. Retry must not silently omit partially imported logical items.
  Render preview details from the same retained values that apply will consume.
- Content export is not a backup: it excludes schedules and study history.
  Preserve active databases and existing destinations on failure or quit.
- Keep irreversible external effects outside retriable transactions. Use
  `unwind-protect` for owned resources; errors and `C-g` must leave safe state.

Before changing persistent encoding, event identity serialization, imports,
exports, or database backup, read
[storage and exchange](docs/storage-and-exchange.md) for compatibility rules,
replacement hazards, and relevant implementation/tests.

## Elisp and verification

Use lexical binding, package-prefixed names, private `--` helpers, accurate
argument/docstring contracts, and native modes, keymaps, hooks, and semantic
faces. Keep optional integrations lazy and dependencies small.

For a bug, add a behavioral ERT regression and observe its expected failure
before fixing it. Refactor while the behavior stays green. Test the public
command or binding as well as the underlying transform when dispatch matters.
Include the failure boundary: stale owner, interrupted write, retry, or mutation
during deferred work. Use small round-trip/invariant tests where examples alone
miss the contract; avoid assertions about incidental helper structure.

For changed interactive flows, exercise the public command and affected keymap
in an interactive Emacs with disposable data. Check the visible result, cancel
path, and refresh/resume behavior where relevant; ERT alone does not establish
usability. Do not load development code into a learner's session as a test.

Run `make JOBS=4 dev` for lint, compilation, autoload checks, ERT,
and the manual.
The Makefile enters the pinned Nix environment when available. Use fresh
bytecode and disposable HOME/XDG/data directories for manual probes; never
create test grades or exercise destructive paths on a learner's database.
Review the complete diff and relevant callers before committing. Keep fixes
and wider behavior-preserving refactors distinguishable, with matching tests.

Maintain this file when ownership or development commands change. Keep durable
invariants here, detailed hazards in the linked reference, and individual bug
cases in regression tests; do not append a new rule for every fix.
