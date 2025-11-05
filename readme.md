# Greader

Greader is an Emacs package that reads the content of the current buffer aloud, keeping the point in sync with the spoken text. It integrates with various text-to-speech engines and provides several features to enhance the reading experience.

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

**Basic Functionality:**

*   When called, `greader-read` starts reading the buffer's content from the current cursor position.
*   The reading proceeds sentence by sentence, advancing the cursor as it goes.

**Prefix Argument:**

*   You can call `greader-read` with a prefix argument (e.g., `C-u C-r <spc>`).
*   When you do this, the cursor will jump to the position where you last started a reading session before the current one, and then it will start reading from there.
*   This is useful for quickly returning to a previous reading position.
*   This functionality relies on a register that stores the last reading position, and it can be disabled by setting the `greader-use-prefix` variable to `nil`.

**Reading a Region:**

*   If you select a region of text before calling `greader-read`, Greader will only read the selected text.
To read the region, greader uses buffer narrowing, so the navigation
keys will only work in the currently displayed buffer region. For a
similar but alternative version, see `greader-queue-mode`.

**Other Features:**

*   **Tired Mode:** `greader-read` integrates with a "tired mode" that can be toggled on and off. When tired mode is active, Greader can automatically adjust its behavior for evening reading.
*   **Timer:** The command also works with a timer, allowing you to read for a set amount of time.

## Minor Modes

Greader provides several minor modes to extend its functionality:

*   **`greader-continuous-mode`:** Automatically turn pages in modes like `info-mode`.
    This minor mode allows Greader to automatically turn pages in multi-page modes. When active, instead of stopping at the end of the buffer, Greader will call the appropriate function to display the next page and continue reading.

    Greader tries to automatically guess the function to call to turn the page. It does this by checking the command bound to the `SPC` key. This works in many modes, like `info-mode`.

    You can customize this behavior for other modes by using the `greader-continuous-modes` variable. This variable is an alist that maps major modes to the function that should be called to turn the page.

    For example, to make `greader-continuous-mode` work with `nov-mode`, you would add the following to your configuration:

    ```emacs-lisp
    (add-to-list 'greader-continuous-modes '(nov-mode . nov-next-document))
    ```

    You can also exclude certain major modes from this behavior using the `greader-continuous-excluded-modes` variable.

    If the page-turning function in a mode is not bound to `SPC`, you can specify a different key with the `greader-continuous-key` variable.
*   **`greader-translate-mode`:** On-the-fly translation of the buffer.
*   **`greader-enriched-mode`:** Announce links and other clickable elements.
*   **`greader-dict-mode`:** Use custom dictionaries for pronunciation.
*   **`greader-study-mode`:** Repeatedly read a buffer or region.
*   **`greader-estimated-time-mode`:** Display estimated reading time.
*   **`greader-auto-bookmark-mode`:** Automatically save a bookmark when you stop reading.

### `greader-translate-mode`

Il `greader-translate-mode` è un minor mode che permette la traduzione "al volo" del buffer che si sta leggendo. Quando questa modalità è attiva, ogni frase viene tradotta prima di essere inviata al motore di sintesi vocale.

#### Utilizzo

1.  Abilitare il minor mode con `M-x greader-translate-mode`.
2.  Avviare la lettura con `greader-read` (`C-r <spc>`). Il testo verrà tradotto e letto nella lingua di destinazione.

#### Configurazione

Perché la traduzione funzioni, è necessario configurare alcune variabili:

*   **`greader-translate-lang-src`**: Imposta la lingua di origine del testo. Questo valore deve essere un codice lingua valido (es. `"en"` per l'inglese, `"fr"` per il francese).

    ```emacs-lisp
    (setq greader-translate-lang-src "en")
    ```

*   **Lingua di destinazione**: La lingua in cui tradurre viene impostata tramite il comando `greader-set-language` (`C-r l`), che a sua volta imposta la lingua per il backend di sintesi vocale.

*   **`greader-translate-function`**: Permette di specificare la funzione da utilizzare per la traduzione. L'impostazione predefinita è `greader-translate-with-google`, che si affida al pacchetto `google-translate`.

*   **`greader-translate-timeout`**: Specifica il tempo massimo in secondi per attendere una risposta dal servizio di traduzione prima di generare un errore. Il valore predefinito è 30 secondi.

#### Prerequisiti e Implicazioni

*   **Dipendenze**: Questa funzionalità richiede l'installazione del pacchetto `google-translate` e delle sue dipendenze.
*   **Connessione Internet**: È necessaria una connessione a Internet per contattare i servizi di traduzione.
*   **Privacy**: L'utilizzo di `greader-translate-with-google` implica l'invio del testo del buffer a servizi esterni (come Google Translate). Questo potrebbe avere implicazioni per la privacy. Gli utenti sono soggetti ai termini di servizio e alle politiche sulla privacy del fornitore del servizio di traduzione. Si consiglia di essere consapevoli di quali dati vengono inviati.

## Customization

You can customize Greader by setting variables in your Emacs configuration file. Use `M-x customize-group RET greader RET` to see the available options.

Some of the customizable variables are:

*   `greader-backends`: A list of available TTS backends.
*   `greader-current-backend`: The default TTS backend to use.
*   `greader-timer`: The default duration for the reading timer.
*   `greader-auto-tired-mode-time`: The time to automatically enable tired mode.

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
