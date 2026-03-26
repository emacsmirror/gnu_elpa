# Greader — Project Guide for Claude

## Overview

**Greader** (v0.15.0) is an Emacs text-to-speech package that reads buffer contents aloud while moving the cursor in sync. It supports multiple TTS backends and provides a rich set of reading modes and helpers.

- **Author**: Michelangelo Rodriguez <michelangelo.rodriguez@gmail.com>
- **License**: GPLv3
- **Requires**: Emacs 26.1+, seq 2.24+, compat 29.1.4.5+
- **Repository**: https://gitlab.com/michelangelo-rodriguez/greader

---

## File Structure

| File | Purpose |
|------|---------|
| `greader.el` | Core module: reading engine, modes, hooks, backend dispatch |
| `greader-dict.el` | Dictionary module: custom pronunciation substitution rules |
| `greader-espeak.el` | eSpeak/eSpeak NG TTS backend |
| `greader-speechd.el` | Speech Dispatcher backend |
| `greader-piper.el` | Piper neural TTS backend (via shell script) |
| `greader-mac.el` | macOS native `say` command backend |
| `greader-audiobook.el` | Convert buffers to audio files (WAV/MP3/etc.) |
| `greader-translate.el` | Translate text before reading (via google-translate) |
| `greader-dict-tests.el` | ERT tests for greader-dict |
| `greader-autoloads.el` | Auto-generated autoloads |
| `greader-pkg.el` | Auto-generated package descriptor |
| `readme.md` | User documentation |
| `greader.texi` | Texinfo source for the Info manual (compiled to `.info` at build time) |
| `piper.sh` | Wrapper script for Piper TTS |

---

## Architecture

Greader uses a **pluggable backend system**. The core engine extracts text chunks, applies transformations (dict substitutions, translation), builds backend args, then calls the active backend asynchronously.

### Data Flow

```
greader-read
  → get sentence (greader-read-chunk-of-text)
  → apply greader-after-get-sentence-functions
      ├─ greader-dict substitutions (if greader-dict-mode)
      └─ greader-translate (if greader-translate-mode)
  → build args (greader-build-args)
  → greader-read-asynchronous → backend process
  → process sentinel → greader-next-action
      ├─ run greader-after-read-hook
      ├─ advance cursor (greader-move-to-next-chunk)
      └─ call greader-read (loop) or finish
```

### Backend Protocol

Each backend is a function that responds to `command` symbols:
- `executable` — return path to TTS binary
- `lang` — get/set language
- `rate` — get/set speech rate
- `punctuation` — toggle punctuation
- `extra` — additional flags
- `get-language` — return current language string
- `get-rate` — return current rate as a number
- `audio-write` — write WAV to file for audiobook generation; arg is `(list TEXT FILENAME)`;
  returns exit code (0 = success) or `'not-implemented` if unsupported.
  **Supported by**: espeak, mac, piper. **Not supported**: speechd.

---

## Minor Modes

| Mode | Purpose |
|------|---------|
| `greader-mode` | Main mode; enables reading |
| `greader-reading-mode` | Active during reading; reading keybindings |
| `greader-dict-mode` | Enable dictionary substitutions |
| `greader-auto-bookmark-mode` | Auto-save position on stop |
| `greader-continuous-mode` | Auto-advance pages (info, nov, etc.) |
| `greader-study-mode` | Loop reading from start/region |
| `greader-enriched-mode` | Announce links and text attributes |
| `greader-estimated-time-mode` | Show estimated reading time |
| `greader-translate-mode` | Enable auto-translation before reading |
| `greader-queue-mode` | Queue text regions in current buffer for sequential reading |
| `greader-compile-mode` | Edit and recompile eSpeak-NG voice dictionaries |
| `greader-timer-mode` | Stop reading after `greader-timer` minutes (`C-r t`) |
| `greader-tired-mode` | After timer expiry, intercept any key press to resume reading; if idle for `greader-tired-time` seconds, move point back to start (`C-r s`); implicitly enables `greader-timer-mode` |
| `greader-auto-tired-mode` | Auto-enable tired mode in the configured night window |
| `greader--tired-intercept-mode` | Transient buffer-local mode (internal); active during the tired wait; binds every key to `greader--tired-wakeup` to resume reading and swallow the original command |

---

## Key Keybindings

All commands are under the prefix `C-r` (configurable via `greader-keymap-prefix`).

| Binding | Action |
|---------|--------|
| `C-r SPC` | Start reading |
| `SPC` | Stop reading (in reading-mode) |
| `C-r b` | Change backend |
| `C-r l` | Set language |
| `C-r t` | `greader-timer-mode` — toggle timer |
| `C-r s` | `greader-tired-mode` — toggle tired/relax mode |
| `C-r f` | Get text attributes |
| `←` / `→` | Navigate backward/forward sentence |
| `p` | Toggle punctuation |
| `.` | Stop with timer |
| `+` / `-` | Increase/decrease speech rate by 10 WPM |

**greader-dict-mode keybindings** (under `C-r d`):

| Binding | Action |
|---------|--------|
| `C-r d a` | Add dictionary entry |
| `C-r d k` | Remove entry |
| `C-r d m` | Modify key |
| `C-r d c` | Change dictionary visibility (global/mode/buffer) |
| `C-r d l` | Pronounce word in other language |
| `C-r d s` | Save dictionary |
| `C-r d i` | Show dictionary info |
| `C-r d M` | Merge an auxiliary dictionary |
| `C-r d f a` | Add filter (regex) |
| `C-r d f k` | Remove filter |
| `C-r d f m` | Modify filter |
| `M-x greader-dict-filters-mode` | Toggle filters on/off (minor mode) |

---

## Key Hooks

| Hook | When fired |
|------|-----------|
| `greader-before-read-hook` | Before reading starts |
| `greader-after-read-hook` | After each sentence is read |
| `greader-after-get-sentence-functions` | To transform sentence text (abnormal hook) |
| `greader-before-finish-functions` | At end of buffer (return non-nil to prevent finish) |
| `greader-after-stop-hook` | After reading stops |
| `greader-after-change-language-hook` | After language changes |

---

## greader-dict Module

Pronunciation dictionary with three entry types:
- **word** — whole-word substitution
- **match** — substring substitution (works inside compound words)
- **filter** — regex substitution (most flexible)

Dictionary visibility levels:
- **global** — applied everywhere
- **mode** — applied to all buffers with a given major-mode
- **buffer** — applied to current buffer only

Dictionaries are stored as files in `greader-dict-directory`.

### Dictionary Merging

Auxiliary dictionaries can be merged into the active one with
`greader-dict-merge-dictionary` (`C-r d M`). Merged entries are marked
via the `greader-dict-merged` text property and are **never written** to
the main dictionary file (`greader-dict-write-file` skips them). Merge
configurations are stored in `greader-dict-merge-dictionaries-alist` and
optionally persisted to `greader-dict-merge-file`. On `greader-dict-mode`
startup, `greader-dict-load-merges` and `greader-dict-merge--dictionaries`
reload saved merges automatically.

Key functions:
- `greader-dict-merge-dictionary` — interactive entry point (`C-r d M`)
- `greader-dict-merge--dictionaries` — internal; reads all auxiliaries from the alist
- `greader-dict-merge-save-to-file` — persists the alist to disk
- `greader-dict-load-merges` — loads the alist from disk

### greader-dict internals — known pitfalls

- `greader-dict-toggle-filters` is **obsolete** (since 0.14); use `greader-dict-filters-mode`.
- `greader-dict-directory` is **not** copied into `with-greader-dict-temp-buffer`. Inside the macro, it starts at its default value (computed at load time, typically the system language). `greader-dict--get-file-name` self-corrects by comparing the path component against `greader-dict-local-language`, but only if that variable is correct.
- `greader-dict-filters-apply` applies filters **sequentially**: filter N sees the output of filter N-1. Do **not** refactor this into a single combined-regex pass — it would change the specified semantics.
- `greader-dict--update` is hooked into `buffer-list-update-hook`, which fires very frequently during reading (every time the TTS process creates/destroys buffers). Both the `greader-dict-mode` and `greader-dict-filters-mode` branches must keep their `(unless greader-reading-mode ...)` guard to avoid spurious reloads.
- `greader-dict--get-key-from-word` accepts an optional pre-computed `matches` list. `greader-dict-check-and-replace` passes it to avoid rebuilding the match list on every word in the sentence.

---

## Testing

Tests use **ERT** (Emacs Regression Testing). Run with:

```
M-x ert-run-tests-interactively
```

Tests are in `greader-dict-tests.el` (covers dict functionality).

---

## Backend Notes

- **eSpeak**: Most portable; good for many languages; rate in WPM. Only backend with
  reliably optimal performance for audiobook generation on long/complex texts.
- **Speech Dispatcher**: Middleware layer; can manage the daemon automatically.
  **Does not support `audio-write`** — `spd-say` has no WAV file output.
- **Piper**: Neural TTS; requires `piper.sh` wrapper script; high quality but can hang or
  produce corrupt audio on long sentences. `piper.sh` accepts an optional second argument
  (output WAV path) to support `audio-write` without duplicating model config in Elisp.
- **macOS (`say`)**: No external dependencies on macOS; supports voice selection. Neural
  voices (e.g. Siri) can hang indefinitely on very long texts — `call-process` is
  synchronous so a hung backend freezes Emacs; `C-g` is the only recovery.

---

## Development Notes

- All files use `lexical-binding: t`
- Package uses `compat` library for backward compatibility
- `greader-dict` checks feature availability with `featurep` before accessing its variables (see commit `9e6acfd`)
- Audiobook generation requires `ffmpeg` for transcoding beyond WAV
- Translation requires the `google-translate` Emacs package
- Use `(or load-file-name (find-library-name "greader"))` — never bare `find-library-name` — when computing file paths at load time; `find-library-name` fails while the file is being loaded (issue #6)
- `greader-ensure-point-visible` is hooked into `greader-before-read-hook` to keep the cursor visible as reading advances (issue #9)
- Piper backend: speech rate is controlled by `greader-piper-rate` (`defcustom`, default 130); `get-rate` command is implemented (issue #7)
- ELPA release is triggered automatically by bumping `Version:` in `greader.el`; `greader-pkg.el` and `greader-autoloads.el` are auto-generated — never edit them manually
- `.info` files are **build artifacts** — excluded from git via `.gitignore` (`*.info`),
  following the GNU Emacs convention. Only `greader.texi` (the Texinfo source) is tracked.
  Generate with: `makeinfo greader.texi -o greader.info`
- `greader-tired-mode` internals: the wakeup intercept uses `greader--tired-intercept-mode`, a transient buffer-local minor mode with a `[t]` catch-all keymap. Any key press calls `greader--tired-wakeup` (swallowing the command) and resumes reading. With `greader-soft-timer` on, the idle timer is armed in `greader--default-action` (after the last sentence finishes) via `greader--tired-pending`, not in `greader-stop-timer-callback`. `greader-stop` clears `greader--tired-pending` to prevent spurious restarts. All cleanup paths go through `greader--tired-cleanup`.
- `greader-audiobook-convert-block` uses `greader-call-backend 'audio-write` — do not
  hardcode espeak-ng there. Error handling: non-zero exit code + WAV size check
  (`greader-audiobook-min-wav-size`). Hard-error policy: `greader-audiobook-on-error`
  (stop/skip/ask). Expected-size deviation check: `greader-audiobook-on-size-mismatch`
  (ignore/warn/error/retry/ask/function); tolerance via
  `greader-audiobook-size-check-tolerance`; min-words threshold via
  `greader-audiobook-size-check-min-words` (blocks below threshold are skipped
  silently); retry limit via `greader-audiobook-size-mismatch-max-retries`.
  Error buffer name derived via
  `greader-audiobook--backend-error-buffer`.
