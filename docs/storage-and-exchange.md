# Storage and exchange contracts

Read this before changing persistent encoding, event identity serialization,
imports, exports, or database backup. These are preservation requirements;
inspect the named implementations and their callers before changing a contract.
Use disposable databases for all probes, including failure and retry tests.

## Database release boundary

Until 0.11.0 is published, released 0.10.6/schema 8 is the only migration
source. Fresh databases use schema 9; the single 8-to-9 transaction creates
the complete FSRS, practice, session-history and accepted-alias layout.
Extend this migration and fresh creation together, not a chain of private
schema versions. Unknown/private layouts fail closed and need a separately
verified conversion; changing `user_version` is not migration validation.

The released fixture in `tests/gnosis-test-schema-v8.el` copies the actual
0.10.6 declarations. Migration retains content (including known archive
columns), links, tags and node rows. Known due dates, repetitions, lapses and
suspension become scheduler baselines/state; daily activity is aggregated.
Legacy algorithm internals are not FSRS memory, and no historical grades are
invented. The old review/activity tables are removed only inside the same
transaction. Errors and quits restore the complete source schema and rows.

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

## Accepted aliases and content format 3

Schema 9 adds nullable `themata.accepted_aliases` TEXT, exposed as the Lisp
column `accepted-aliases`. The owned SQL serializer encodes a list of strings;
NULL/nil means no aliases. Migration uses ALTER TABLE and leaves canonical
answers, retained extra columns and study evidence intact. `gnosis-answer`
owns validation, matching and the alias list-section codec; it does not own
storage or review acceptance. Canonical reveal and input-method selection do
not use aliases. Only basic, image-occlusion and model-name accept nonempty
aliases; double authoring puts them on the forward basic thema only.

`gnosis-add-thema-fields` takes trailing optional ACCEPTED-ALIASES after
REVIEW-IMAGE and GNOSIS-ID. `gnosis-update-thema` takes it after TYPE: omission
preserves retained aliases, explicit nil clears them. Domain save handlers
preserve this supplied/omitted distinction. A full native draft is replacement
content: missing or empty `** Accepted aliases` clears aliases on save. Its
items use one `- ` marker per single-line spelling, independent of Answer.
The parser identifies fields by heading and rejects duplicate/unknown fields;
parsed positions 0–7 retain their old meaning and index 8 carries aliases.

Portable content format 3 carries aliases as an explicit column. Formats 1/2
remain readable with missing aliases normalized to nil. Alias-only updates and
clears must appear in preview, and both source and destination snapshots guard
against alias drift. None of these formats bundles managed media: image types,
model Find/Name and managed syntax remain refused. Preserve this distinction
from schema 9 and database-plus-assets backup format 1.

Start with `gnosis-test-answer`, `gnosis-test-aliases-codec` and
`gnosis-test-media-integration`. The latter retains Name recursive-input faults
and pending alias drift through scheduled/practice acceptance. Alias snapshots
must survive outcome overrides; refusal must not create study evidence.

## Managed images

`gnosis-image` supplies `image.json` and one PNG/JPEG basename to the shared
asset publisher. Both manifest versions retain raster dimensions and optional
source/attribution strings. Version 1 has labelled regions with unique stable
IDs and one normalized `[x, y, width, height]` `rect`; version 2 uses a nonempty
`rects` list per target. Never mix `rect` and `rects` within a target. Legacy
single-rectangle imports keep version 1; plural input emits version 2 for all
targets. The manifest is bounded to 128 KiB, 255 targets and 255 total rectangles;
no targets is valid for an ordinary inline image. IDs and labels belong to the
target, not its rectangles. Region edits publish a new byte revision; existing
references keep their original raster and regions.

Ordinary content uses `[[gnosis-image:<revision>/image.json]]` Org links.
`image-region` uses one resource string in `hypothesis` and one stable target
string in `answer`; all sibling rectangles select that ID. `image-occlusion`
uses `(RESOURCE TARGET POLICY)` in `hypothesis` and `(TEXT)` in `answer`.
POLICY is exactly `"hide-target"` or `"hide-all"`; old one/two-field hypotheses
use hide-target. TEXT is a nonempty editable human answer checked with ordinary
Gnosis text comparison and its independent accepted aliases. Hide-target masks
every rectangle of the tested ID; hide-all masks all annotated rectangles.
Neutral tested-target cues are drawn after opaque masks; neither policy
provides OCR or protection against unannotated answer text. Reveal displays
only the original raster, without overlays. Historical
occlusion rows with `(RESOURCE)` / `(TARGET)` remain valid: the immutable
manifest's target label supplies the expected text, never the raw stable ID.
Opening such a native draft exposes the canonical fields; saving that draft
persists them. No bulk migration or asset rewrite occurs. New attachments
prefill text from the region label; reattaching a canonical draft preserves
its edited answer. The image resource heading uses the normal list codec. Header
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

Find (`model`) retains `(RESOURCE YAW PITCH ZOOM)` strings in `hypothesis`
and one stable target ID in `answer`. Name (`model-name`) instead stores
`(RESOURCE TARGET YAW PITCH ZOOM)` in `hypothesis` and one canonical text in
`answer`; aliases use the independent schema-9 column. `gnosis-model-fields`
validates both modes against the immutable scene before saving or using them.

Unversioned scenes project object targets in memory without rewriting bytes.
Version 2 stores explicit targets separate from mesh objects: each has `id`,
`label`, `mesh` and `kind`. Object targets need no geometry; point targets have
zero-based `face`, three barycentric weights and positive original-coordinate
`tolerance`; region targets have distinct nonempty `faces`. OBJ fan-triangle
identity and original coordinates are shared with the C3D2 renderer, not
reconstructed from object-index bytes. Points match Euclidean radius on the
same mesh; regions match faces. Candidate resolution considers the expected
kind, with nearest-point then stable-ID tie-breaking. Keep topology validation
and target interpretation in `gnosis-model`; rendering/picking never grades.
Target authoring publishes a new revision, preserving old cards and hashes.
The limits are 255 objects, 4096 targets, 64-KiB scene JSON, 100 MB per OBJ and
two million triangles. No textures/materials or topology editor are supplied.
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
