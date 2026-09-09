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

## Managed images

`gnosis-image` supplies `image.json` and one PNG/JPEG basename to the shared
asset publisher. The manifest contains version 1, raster dimensions, optional
source/attribution strings, and labelled regions with unique stable IDs and
normalized `[x, y, width, height]` rectangles. Region edits publish a new byte
revision; existing references keep their original raster and regions.

Ordinary content uses `[[gnosis-image:<revision>/image.json]]` Org links.
`image-region` and `image-occlusion` use one resource string in `hypothesis`
and one stable target string in `answer`, without a schema change. Header
validation permits standard PNG color/depth combinations and 8-bit grayscale
or RGB JPEG, within size limits; it does not establish compressed-pixel
validity. Native decoding is additionally required for study. Missing,
corrupt or unsupported media is unavailable, never an incorrect answer.

Pending image results retain all content fields, including extras, plus the
original database and encounter. Preserve both image and model guards through
outcome overrides and duplicate acceptance. Practice does not alter FSRS;
scheduled acceptance and undo use the existing event machinery.

Content-only SQLite exchange rejects image types and managed syntax in every
content field, including malformed references and legacy extras. Org drafts
retain references but do not bundle their bytes. Back up the database and its
adjacent `assets` directory together; copying only the database loses media.

## Model resources

Model themata reuse the existing thema schema and owned Lisp serializer:
`hypothesis` contains (RESOURCE YAW PITCH ZOOM) strings and `answer` contains
one stable target ID.  `gnosis-model` validates the version-addressed scene
and every listed OBJ before saving or using it; no schema migration is needed.
Scheduled models use the same atomic event/state acceptance and replay as
other themata; practice writes no scheduled evidence.  Pending model results
retain database, encounter and thema identity through outcome overrides and
validate the pinned resources again before acceptance.
The new discriminator does not reinterpret historical thema data.  Authoring
shows a resource/view heading, not a hint.  Assets live at
`assets/<sha256>/scene.json` beside the actual connected database; the manifest
owns targets and attribution.  Geometry and manifest bytes define the revision.
`gnosis-assets` owns file checks and publication; domain modules supply the
explicit set of basenames. Revisions hash the owned Lisp serialization of
sorted unique `(basename bytehash)` pairs, never arbitrary directory contents.
Scene hash preimages and the existing on-disk layout remain unchanged. The
asset root uses the canonical parent of the actual connected database, not
`gnosis-dir`; legitimate symlinked configuration ancestry is allowed, but
asset roots, revision directories and source files must not be symlinks.

Import stages and validates before rename; exact retries share a revision.
Resources are pinned before target/camera prompts.  Cancelled authoring can
leave unreferenced immutable assets but no partial thema.  Do not overwrite or
automatically garbage-collect shared revisions.  Content SQLite export/import
refuses model resources until a portable bundled format is supported.  Org
editing preserves the reference but cannot transfer the assets.  The legacy
`gnosis-backup-db` and automatic database Git commits do not include `assets/`;
`gnosis-backup-data` includes the entire managed subtree.

## Content export and database backup

`gnosis-export-db` in `lisp/gnosis-export-import.el` exports thema content, not
schedules or study history. `gnosis-backup-db` in `lisp/gnosis-study.el`
creates a SQLite snapshot retaining database evidence, not a backup of Org/media
files. Keep that distinction explicit in commands and documentation.

`lisp/gnosis-backup.el` owns versioned database-plus-managed-media snapshots.
`gnosis-backup-data` captures an already-connected file-backed connection and
its PRAGMA main path, never opening or migrating based on configuration.
VACUUM INTO runs outside retriable transactions. The new directory contains
`database.sqlite`, the entire `assets/` subtree (including unreferenced bytes
and empty directories), and `manifest.json`. Org source vaults and external
media remain separate backups; this command is not a full notes backup.
Legacy `[[file:...]]` and `extras.review-image` paths are retained as stored,
not followed or copied. Do not crawl arbitrary external paths.

Version 1 manifests are JSON arrays `["gnosis-data", 1, entries]`, with sorted
`[relative-path, "directory"]` and `[relative-path, "file", byte-length,
lowercase-sha256]` entries. Only the root manifest excludes itself. Inventory
equality against independently walked safe local paths rejects duplicate,
missing, extra and conflicting entries without trusting manifest paths for
copying. The manifest is bounded to 16 MiB; inventory to 100,000 entries,
32 path components and 1,024 path characters. Managed simple basenames only;
reject symlinks and special files. Do not normalize or rewrite asset bytes.

`gnosis-backup-verify` requires byte closure, SQLite header, physical integrity
and foreign-key integrity, not domain semantics, schema compatibility or
authenticity. Emacs 29/30 SQLite opening has no read-only option; integrity
checks use a private copy to avoid source-side WAL companions. Model/image
resolvers remain the owners of semantic and referenced-resource validation.

`gnosis-backup-restore` verifies before copying and rechecks the copied snapshot.
Snapshot and restore publish verified private sibling stages by no-overwrite
rename into new destinations only. Check overlap with the source snapshot,
active main database, companion names and asset root before staging, including
an absent asset root; never merge, activate or replace in place. Pin connection
identity through callbacks, re-inventory sources after copying/verification,
and clean only the owned unpublished stage on error or quit. These repeated
checks detect observed drift, not hostile concurrent filesystem mutation;
require a trusted, quiescent local filesystem. Restored `database.sqlite` is
deliberately not auto-activated as `gnosis.db`. See the manual for a separate,
disconnected cutover and `tests/gnosis-test-backup.el` for recovery regressions.

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
