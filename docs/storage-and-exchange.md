# Storage and exchange contracts

Read this before changing persistent encoding, event identity serialization,
imports, exports, or database backup. These are preservation requirements;
inspect the named implementations and their callers before changing a contract.
Use disposable databases for all probes, including failure and retry tests.

## Lisp encoding

`lisp/gnosis-sqlite.el` owns Lisp/SQL encoding. Its
`gnosis-sqlite--serialize` binds printer settings so retained values do not
inherit display truncation, escaping, or continuous object numbering.
SQL parameter encoding is distinct: nil becomes SQL NULL, numbers pass through,
and other values use the serializer. Preserve nil versus empty text.

Route bulk writes and serialized event-identity preimages through the owned
serializer, rather than open-coding `prin1-to-string` at callers. Preserve
existing nested-string encodings instead of adding or removing a layer. Inspect
Anki field encoding, node tags, bulk tag insertion, and scheduler event IDs when
changing this shared contract.

Start with `tests/gnosis-test-sqlite.el` and
`tests/gnosis-test-printer-callers.el`; database reopen coverage also lives in
`tests/gnosis-test-db-safety.el`. Test adverse printer settings and round trips,
not just the displayed representation.

## Import preview and apply

`lisp/gnosis-export-import.el` retains source identity, destination identity and
relevant content, and detail values for the reviewed import. Apply must validate
both source and destination rather than trusting whichever database is current.
Render details from the retained values that apply will consume. A changed
source or destination must not silently turn an accepted preview into a
different write.

`lisp/gnosis-anki.el` owns Anki normalization and import application.
Preserve the logical item across failures: retry must not skip content because
part of a note was already imported. Keep irreversible Git/file/process effects
outside retriable database transactions.

For preview drift and partial-import retry coverage, start with:
- `tests/gnosis-test-export-import.el`
- `tests/gnosis-test-exchange-hardening.el`
- `tests/gnosis-test-authoring-hardening.el`

## Model resources

Model themata reuse the existing thema schema and owned Lisp serializer:
`hypothesis` contains (RESOURCE YAW PITCH ZOOM) strings and `answer` contains
one stable target ID.  `gnosis-model` validates the version-addressed scene
and every listed OBJ before saving or using it; no schema migration is needed.
The new discriminator does not reinterpret historical thema data.  Authoring
shows a resource/view heading, not a hint.  Assets live at
`assets/<sha256>/scene.json` beside the actual connected database; the manifest
owns targets and attribution.  Geometry and manifest bytes define the revision.

Import stages and validates before rename; exact retries share a revision.
Resources are pinned before target/camera prompts.  Cancelled authoring can
leave unreferenced immutable assets but no partial thema.  Do not overwrite or
automatically garbage-collect shared revisions.  Content SQLite export/import
refuses model resources until a portable bundled format is supported.  Org
editing preserves the reference but cannot transfer the assets.  Full database
backup and automatic database Git commits do not include `assets/`.

## Content export and database backup

`gnosis-export-db` in `lisp/gnosis-export-import.el` exports thema content, not
schedules or study history. `gnosis-backup-db` in `lisp/gnosis-study.el`
creates a SQLite snapshot retaining database evidence, not a backup of Org/media
files. Keep that distinction explicit in commands and documentation.

For content-export replacement:

- Reject aliases of the active database and its SQLite companions, including
  symlinks and hard links. Protect companion paths even before they exist.
- Refuse single-file replacement when the destination has `-wal`, `-shm`, or
  `-journal` companions. Do not delete or checkpoint another owner's files to
  make replacement succeed.
- Build a private sibling file, detach it, and validate its format, integrity,
  foreign keys, and expected content before replacing the destination.
- Check the destination again immediately before rename. Preserve the original
  on error or quit. Clean up owned temporary resources with `unwind-protect`.
- These checks are not cross-process exclusion; do not describe them as a lock
  against concurrent writers.

Inspect `gnosis-export--check-destination`, `gnosis-export--validate`, and
`gnosis-export--replace-file` together. Replacement failure, invalid completion,
alias, and companion tests live in `tests/gnosis-test-exchange-hardening.el`.
The backup/content-export distinction is tested in `tests/gnosis-test-study.el`.
