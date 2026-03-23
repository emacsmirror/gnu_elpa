# Greader

Greader is an Emacs package that reads the content of the current buffer aloud, keeping the point in sync with the spoken text. It integrates with various text-to-speech engines and provides several features to enhance the reading experience.

## Table of Contents

*   [Main Features](#main-features)
*   [Installation](#installation)
*   [Basic Usage](#basic-usage)
*   [Keybindings](#keybindings)
*   [Commands](#commands)
*   [Minor Modes](#minor-modes)
*   [Backends](#backends)
*   [Customization](#customization)
*   [License](#license)

## Main Features

*   **Text-to-Speech:** Reads any buffer aloud using different backends.
*   **Synchronized Reading:** The cursor moves along with the text being read.
*   **Multiple Backends:** Supports `espeak`, `speech-dispatcher`, `piper` and macOS's native speech synthesis.
*   **Continuous Mode:** Automatically advances to the next page or section in multi-page modes like `info-mode` or `nov-mode`.
*   **Timer:** Set a timer to read for a specific duration.
*   **On-the-fly Translation:** Translate and read buffers in different languages.
*   **Audiobook Creation:** Convert any buffer into an audiobook.
*   **Enriched Text:** Announce links and other clickable elements in the buffer.
*   **Custom Pronunciation:** Define custom pronunciation rules using dictionaries.
*   **Study Mode:** Repeatedly read a buffer or a region for study purposes.
*   **Estimated Reading Time:** Shows the estimated reading time in the mode line.

## Installation

1.  **Install Dependencies:**
    Greader requires a text-to-speech engine to be installed on your system. You can choose one of the following:
    *   [eSpeak NG](https://github.com/espeak-ng/espeak-ng)
    *   [Speech Dispatcher](http://devel.freebsoft.org/speechd)
    *   [Piper](https://github.com/rhasspy/piper)

2.  **Install Greader:**
    Clone the repository or add it to your Emacs load path.

    ```emacs-lisp
    (add-to-list 'load-path "/path/to/greader")
    (require 'greader)
    ```

## Basic Usage

1.  Start greader mode with `M-x greader-mode`.
2.  Press `C-r <spc>` to start reading.
If desired, you can change the prefix. See the command `greader-set-map-prefix`.
3.  Press `<spc>` to stop reading.

## Keybindings

### General
| Keybinding | Command | Description |
|---|---|---|
| `C-r <spc>` | `greader-read` | Start reading from point. |
| `SPC` | `greader-stop` | Stop reading (only when you are in `greader-reading-mode', which happens when you call `greader-read').|
| `C-r l` | `greader-set-language` | Set the language for the TTS engine. |
| `C-r b` | `greader-change-backend` | Cycle through available backends. |
| `C-r t` | `greader-toggle-timer` | Toggle the reading timer. |
| `C-r s` | `greader-toggle-tired-mode` | Toggle tired/relax mode. |
| `C-r f` | `greader-get-attributes` | Report text attributes at point. |
| `M-x greader-set-timer` | | Set the duration for the timer. |

### Navigation
The following commands work when you are in `greader-reading-mode', which happens when you call `greader-read'.
| Keybinding | Command | Description |
|---|---|---|
| `<left>` | `greader-backward` | Move to the previous sentence. |
| `<right>` | `greader-forward` | Move to the next sentence. |
| `p` | `greader-toggle-punctuation` | Toggle reading of punctuation characters. |
| `.` | `greader-stop-with-timer` | Stop reading and reset the timer. |
| `+` | `greader-inc-rate` | Increase speech rate by 10 WPM. |
| `-` | `greader-dec-rate` | Decrease speech rate by 10 WPM. |

## Commands

### `greader-read`

The `greader-read` command is the core function for starting text-to-speech synthesis in Greader. It can be invoked with `C-r <spc>`.

*   **Basic Functionality:** When called, `greader-read` starts reading the buffer content from the current cursor position, advancing sentence by sentence.
*   **Prefix Argument (`C-u C-r <spc>`):** Jumps to the last reading position and starts reading from there. This can be disabled by setting `greader-use-prefix` to `nil`.
*   **Reading a Region:** If a region is selected, Greader will only read the selected text. Note that this uses buffer narrowing, so navigation is limited to the selected region. For an alternative, see `greader-queue-mode`.
*   **Other Features:** `greader-read` also integrates with "tired mode" for automatic adjustments and a timer for reading for a set duration.

## Minor Modes

Greader provides several minor modes to extend its functionality:

*   **`greader-continuous-mode`:** Automatically turn pages in modes like `info-mode`.
    *   This minor mode allows Greader to automatically turn pages in multi-page modes. When active, instead of stopping at the end of the buffer, Greader will call the appropriate function to display the next page and continue reading.
    *   Greader tries to automatically guess the function to call to turn the page. It does this by checking the command bound to the `SPC` key. This works in many modes, like `info-mode`.
    *   You can customize this behavior for other modes by using the `greader-continuous-modes` variable. This variable is an alist that maps major modes to the function that should be called to turn the page.
    *   For example, to make `greader-continuous-mode` work with `nov-mode`, you would add the following to your configuration:
        ```emacs-lisp
        (add-to-list 'greader-continuous-modes '(nov-mode . nov-next-document))
        ```
    *   You can also exclude certain major modes from this behavior using the `greader-continuous-excluded-modes` variable.
    *   If the page-turning function in a mode is not bound to `SPC`, you can specify a different key with the `greader-continuous-key` variable.
*   **`greader-translate-mode`:** On-the-fly translation of the buffer.
*   **`greader-enriched-mode`:** Announce links and other clickable elements.
*   **`greader-dict-mode`:** Use custom dictionaries for pronunciation.
    `greader-dict-mode` allows you to create custom pronunciation rules for words or patterns. This is useful for correcting mispronunciations by the text-to-speech engine.

    ### How it Works

    You can define two types of dictionary entries:

    *   **Word:** A whole-word substitution. For example, if you define a "word" entry for "Emacs", it will only replace "Emacs" when it appears as a standalone word, not in "EmacsLisp".
    *   **Match:** A literal substring substitution. For example, a "match" entry for "read" could apply to "reading" and "reader". Matches can also be regular expressions for more complex patterns.

    ### Dictionary Visibility

    Greader supports three levels of dictionary visibility:

    *   **`global`:** The default dictionary, applied everywhere.
    *   **`mode`:** A dictionary specific to a major mode (e.g., a dictionary for `org-mode` buffers).
    *   **`buffer`:** A dictionary that applies only to the current buffer.

    You can change the visibility for the current buffer using the `greader-dict-change-dictionary` command (`C-r d c`).

    To set a default visibility for a specific mode, you can add a hook to your Emacs configuration. For example, to use `mode` visibility for `info-mode`:

    ```emacs-lisp
    (add-hook 'info-mode-hook
              (lambda ()
                (greader-dict-mode 1)
                (greader-dict--set-file 'mode)))
    ```

    ### Usage

    **Enable the mode:** `M-x greader-dict-mode`

    | Keybinding | Command | Description |
    |---|---|---|
    | `C-r d a` | `greader-dict-add-entry` | Add entry. No prefix: whole-word; `C-u`: substring match; active region: proposes region as match. |
    | `C-r d k` | `greader-dict-remove-entry` | Remove an entry. |
    | `C-r d m` | `greader-dict-modify-key` | Modify an existing entry key. |
    | `C-r d c` | `greader-dict-change-dictionary` | Change dictionary visibility (global / mode / buffer). |
    | `C-r d l` | `greader-dict-pronounce-in-other-language` | Pronounce a word using a different language. |
    | `C-r d s` | `greader-dict-save` | Save the current dictionary to disk immediately. |
    | `C-r d i` | `greader-dict-info` | Display information about the current dictionary. |

    ### Filters

    Filters are an advanced feature that allows you to use arbitrary regular expressions for substitutions. They are more powerful than "word" or "match" entries but can be less efficient.

    | Keybinding | Command | Description |
    |---|---|---|
    | `M-x greader-dict-filters-mode` | | Toggle filter processing on/off (minor mode). |
    | `C-r d f a` | `greader-dict-filter-add` | Add a new regex filter. |
    | `C-r d f k` | `greader-dict-filter-remove` | Remove a filter. |
    | `C-r d f m` | `greader-dict-filter-modify` | Modify an existing filter. |
*   **`greader-study-mode`:** Repeatedly read a buffer or region.
*   **`greader-estimated-time-mode`:** Display estimated reading time.
*   **`greader-auto-bookmark-mode`:** Automatically save a bookmark when you stop reading.

### `greader-queue-mode`

`greader-queue-mode` lets you assemble a playlist of text regions within the current buffer and read them in sequence. Useful when you want to read only selected parts of a document, or when the region is non-contiguous. When `greader-queue-mode` is active, `greader-mode` is temporarily suspended; it is re-enabled automatically when you disable `greader-queue-mode`.

**Enable:** `M-x greader-queue-mode`

| Keybinding | Command | Description |
|---|---|---|
| `C-r RET` | `greader-queue-add-element` | Add the selected region (or current position) to the queue. |
| `C-r SPC` | `greader-queue-read` | Start reading the queue. |
| `C-r <left>` | `greader-queue-backward` | Move to the previous element in the queue. |
| `C-r <right>` | `greader-queue-forward` | Move to the next element in the queue. |
| `C-r .` | `greader-queue-stop` | Stop reading. |

Additional commands available via `M-x`: `greader-queue-remove-element`, `greader-queue-reset-queue`.

### `greader-compile-mode`

`greader-compile-mode` is for users who want to edit and recompile eSpeak-NG voice dictionaries from within Emacs. It is not needed for ordinary text-to-speech use.

**Prerequisites:** Set `greader-compile-dictsource` to the list of directories containing your eSpeak-NG dictionary source files before enabling this mode:

```elisp
(setq greader-compile-dictsource '("/path/to/espeak-ng-data/dictsource/"))
```

Paths must end with a slash. The variable is a list, so you can specify multiple directories.

**File naming convention:** eSpeak-NG dictionary source files must follow the pattern `XX_suffix`, where `XX` is a two-letter language code (e.g., `en_extra`, `it_rules`). `greader-compile-mode` uses the filename to infer the language to compile.

**Enable:** `M-x greader-compile-mode`

**Auto-compilation on save:** Once enabled, saving any dictionary source file whose directory is in `greader-compile-dictsource` automatically runs `espeak --compile=XX` for the corresponding language. If the eSpeak-NG data directory is not writable by your user, you will be prompted for your administrator password.

| Keybinding | Command | Description |
|---|---|---|
| `C-r c` | `greader-compile-at-point` | Add the word at point to the dictionary and recompile. With a prefix argument, prompts for the word. |

`greader-compile` — Compile eSpeak-NG definitions interactively. With a prefix argument, prompts for the language code; without, infers it from the current file name.

`greader-compile-goto-source` — Visit the dictionary source file currently used by `greader-compile-at-point`.

### `greader-translate-mode`

`greader-translate-mode` is a minor mode that allows for on-the-fly translation of the buffer being read. When this mode is active, each sentence is translated before being sent to the text-to-speech engine.

#### Usage

1.  Enable the minor mode with `M-x greader-translate-mode`.
2.  Start reading with `greader-read` (`C-r <spc>`). The text will be translated and read in the destination language.

#### Configuration

For the translation to work, you need to configure a few variables:

*   **`greader-translate-lang-src`**: Sets the source language of the text. This value must be a valid language code (e.g., `"en"` for English, `"fr"` for French).

    ```emacs-lisp
    (setq greader-translate-lang-src "en")
    ```

*   **Destination Language**: The language to translate to is set via the `greader-set-language` command (`C-r l`), which in turn sets the language for the speech synthesis backend.

*   **`greader-translate-function`**: Allows you to specify the function to use for translation. The default setting is `greader-translate-with-google`, which relies on the `google-translate` package.

*   **`greader-translate-timeout`**: Specifies the maximum time in seconds to wait for a response from the translation service before generating an error. The default value is 30 seconds.

#### Prerequisites and Implications

*   **Dependencies**: This feature requires the installation of the `google-translate` package and its dependencies.
*   **Internet Connection**: An internet connection is required to contact the translation services.
*   **Privacy**: Using `greader-translate-with-google` involves sending the buffer's text to external services (like Google Translate). This may have privacy implications. Users are subject to the terms of service and privacy policies of the translation service provider. It is recommended to be aware of what data is being sent.

## Backends

Switch backends with `C-r b` (`greader-change-backend`) or by setting `greader-current-backend` in your configuration.

### eSpeak / eSpeak NG

The default backend on most systems. Install from your distribution or from [espeak-ng](https://github.com/espeak-ng/espeak-ng). No additional Emacs configuration is required beyond having `espeak-ng` on your `PATH`.

### Speech Dispatcher

Middleware layer that can route to many underlying synthesizers. Greader can start the Speech Dispatcher daemon automatically. See [Speech Dispatcher](http://devel.freebsoft.org/speechd).

### Piper

Neural TTS backend that produces high-quality, natural-sounding speech. Requires two things:

1. The `piper` binary, available from [piper](https://github.com/rhasspy/piper) or via your distribution.
2. The `piper.sh` wrapper script shipped with greader. The script must be executable and accessible on your `PATH` or at the path set in `greader-piper-script-path`.

| Variable | Default | Description |
|---|---|---|
| `greader-piper-script-path` | `piper.sh` in greader directory | Full path to the `piper.sh` wrapper script. |
| `greader-piper-rate` | `130` | Speech rate in words per minute. |

```emacs-lisp
(setq greader-piper-script-path "/usr/local/bin/piper.sh")
(setq greader-piper-rate 150)
```

### macOS (`say`)

Uses the built-in macOS `say` command. No external installation required.

| Variable | Default | Description |
|---|---|---|
| `greader-mac-voice` | `nil` | Voice name to use (e.g., `"Samantha"`). `nil` uses the system default. |
| `greader-mac-rate` | `200` | Speech rate in words per minute. |

```emacs-lisp
(setq greader-mac-voice "Samantha")
(setq greader-mac-rate 180)
```

## Audiobook Creation

The `greader-audiobook-buffer` command converts the current buffer into a collection of WAV
audio files, optionally transcoding them to MP3/M4A/FLAC and bundling them into a ZIP or M4B
audiobook container.

### Backend support

Audiobook generation uses the **current greader TTS backend** (`greader-current-backend`), so
the same voice, language, and rate you use for live reading are applied to the audiobook.
Supported backends:

| Backend | Notes |
|---|---|
| eSpeak / eSpeak NG | **Recommended.** Fast, stable, and reliable on any text. |
| Piper | High-quality neural TTS. Requires `piper.sh` v2 (see below). May be unstable on long or complex texts. |
| macOS `say` | Native macOS speech; outputs 16-bit PCM WAV. **See stability note below.** |
| Speech Dispatcher | **Not supported** — `spd-say` cannot write WAV files. |

**eSpeak is the only backend with consistently optimal performance for audiobook
generation.** AI-based backends (Piper, macOS `say` with neural voices such as Siri) are
designed for short, interactive utterances and can become unreliable when converting long,
complex texts such as books:

- They may produce truncated or garbled audio blocks while reporting success (exit code 0).
  The `greader-audiobook-min-wav-size` check catches the worst cases, but subtle corruption
  may go undetected.
- On very long sentences or paragraphs they may **hang indefinitely**. Because block
  conversion is synchronous, a hung backend freezes Emacs entirely; `greader-audiobook-on-error`
  cannot intervene. Use `C-g` to interrupt and kill the process manually.

If you want to use a neural voice for an audiobook, consider splitting the source buffer into
smaller chunks before converting.

### Piper and audiobook generation

The `piper.sh` wrapper script shipped with greader supports an optional second argument: the
output WAV file path. When present, piper writes to that file instead of playing audio in
real time. No additional Emacs configuration is required; the model path is read from
`piper.sh` as usual.

If you maintain a custom copy of `piper.sh` in your `PATH`, update it to the current version
from the greader repository to enable audiobook support.

### Error handling

AI-based backends can produce silent failures (corrupted or near-empty audio) alongside
outright crashes. Greader detects both:

- **Non-zero exit code** — the backend process failed; the error buffer (`*espeak-output*`,
  `*piper-output*`, `*say-output*`, …) will contain details.
- **WAV too small** — the output file is below `greader-audiobook-min-wav-size` bytes even
  if the backend reported success; the block is treated as corrupt.

The variable `greader-audiobook-on-error` controls what happens when a block fails:

| Value | Behaviour |
|---|---|
| `stop` | Signal an error and abort conversion immediately (default). |
| `skip` | Log the block number and continue with the next block. |
| `ask` | Ask interactively whether to skip or abort. |

When blocks are skipped a summary message lists their numbers at the end of conversion.

### Audiobook customization

Use `M-x customize-group RET greader-audiobook RET` for the full list of options. Key
variables:

| Variable | Default | Description |
|---|---|---|
| `greader-audiobook-base-directory` | `~/.emacs.d/audiobooks/` | Root directory for generated audiobooks. |
| `greader-audiobook-block-size` | `"15"` | Block size: a string means minutes, a number means characters. |
| `greader-audiobook-transcode-wave-files` | `nil` | Transcode WAV blocks via ffmpeg. |
| `greader-audiobook-transcode-format` | `"mp3"` | Target format for transcoding. |
| `greader-audiobook-on-error` | `stop` | Error policy: `stop`, `skip`, or `ask`. |
| `greader-audiobook-min-wav-size` | `1000` | Minimum WAV size in bytes; smaller files are treated as corrupt. |
| `greader-audiobook-create-m4b` | `nil` | Bundle all blocks into a single M4B audiobook file. |
| `greader-audiobook-compress` | `t` | Compress the audiobook directory into a ZIP file. |

## Customization

You can customize Greader by setting variables in your Emacs configuration file. Use `M-x customize-group RET greader RET` to see the available options.

Some of the customizable variables are:

*   `greader-backends`: A list of available TTS backends.
*   `greader-current-backend`: The default TTS backend to use.
*   `greader-timer`: The default duration for the reading timer.
*   `greader-auto-tired-mode-time`: The time to automatically enable tired mode.
*   `greader-soft-timer`: If `t` (the default), finishes the current sentence before stopping at timer expiration. Set to `nil` for a hard cutoff.
*   `greader-backward-acoustic-feedback`: If `t`, plays a brief beep when the cursor returns to the previous reading position after backward navigation. Default `nil`.
*   `greader-backward-seconds`: Number of seconds to wait at the previous sentence before automatically returning to the reading position. Default `5`.
*   `greader-dict-save-after-time`: Idle time in seconds before the dictionary is saved automatically. Default `30`. (Requires `greader-dict-mode`.)

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
