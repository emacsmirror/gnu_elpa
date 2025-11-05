# Greader

Greader is an Emacs package that reads the content of the current buffer aloud, keeping the point in sync with the spoken text. It integrates with various text-to-speech engines and provides several features to enhance the reading experience.

## Table of Contents

*   [Main Features](#main-features)
*   [Installation](#installation)
*   [Basic Usage](#basic-usage)
*   [Keybindings](#keybindings)
*   [Commands](#commands)
*   [Minor Modes](#minor-modes)
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
| `M-x greader-set-timer` | | Set the duration for the timer. |

### Navigation
The following commands work when you are in `greader-reading-mode', which happens when you call `greader-read'.
| Keybinding | Command | Description |
|---|---|---|
| `<left>` | `greader-backward` | Move to the previous sentence. |
| `<right>` | `greader-forward` | Move to the next sentence. |

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

    *   **Enable the mode:** `M-x greader-dict-mode`
    *   **Add an entry:** `C-r d a` (`greader-dict-add-entry`).
        *   With no prefix, it adds a "word" entry.
        *   With a prefix argument (`C-u`), it adds a "match" entry.
        *   If a region is active, it will propose adding the selected text as a "match".
    *   **Remove an entry:** `C-r d k` (`greader-dict-remove-entry`).
    *   **Modify a key:** `C-r d m` (`greader-dict-modify-key`).
    *   **Change dictionary visibility:** `C-r d c` (`greader-dict-change-dictionary`).

    ### Filters

    Filters are an advanced feature that allows you to use arbitrary regular expressions for substitutions. They are more powerful than "word" or "match" entries but can be less efficient.

    *   **Toggle filters:** `M-x greader-dict-toggle-filters`
    *   **Add a filter:** `greader-dict-filter-add`
    *   **Remove a filter:** `greader-dict-filter-remove`
    *   **Modify a filter:** `greader-dict-filter-modify`

    ### Pronounce in Another Language

    You can use `greader-dict-pronounce-in-other-language` (`C-r d l`) to hear how a word is pronounced in a different language, using your configured TTS backend.
*   **`greader-study-mode`:** Repeatedly read a buffer or region.
*   **`greader-estimated-time-mode`:** Display estimated reading time.
*   **`greader-auto-bookmark-mode`:** Automatically save a bookmark when you stop reading.

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

## Customization

You can customize Greader by setting variables in your Emacs configuration file. Use `M-x customize-group RET greader RET` to see the available options.

Some of the customizable variables are:

*   `greader-backends`: A list of available TTS backends.
*   `greader-current-backend`: The default TTS backend to use.
*   `greader-timer`: The default duration for the reading timer.
*   `greader-auto-tired-mode-time`: The time to automatically enable tired mode.

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
