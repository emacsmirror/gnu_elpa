# dmsg.el

Structured debug logging for Emacs.  Each `(dmsg ...)` call appends a
timestamped, levelled entry, with backtrace to a dedicated buffer and provides
a custom `dmsg-mode` to interact with it.



## Installation

```elisp
(add-to-list 'load-path "/path/to/dmsg/")
(require 'dmsg)
```



## Usage

```elisp
(dmsg "x is %S" x)               ; debug level (default)
(dmsg 'warn "unexpected: %=s" x)   ; explicit level
(dmsg 'error "failed: %s" msg)

;; %=X logs label=value automatically
(let ((state 'idle)
      (count 10))
  (dmsg "Status: %=S %=d" state count))
;; [DBG] [2026-01-01 12:00:00.123] [eval] States: state=idle count=42

;; Switch to buffer
(display-buffer dmsg-buffer-name)
```

Levels in increasing severity: `debug` `info` `warn` `error`.



## Buffer format

```
* LVL [YYYY-MM-DD HH:MM:SS.mmm] first line of message
 continuation line                    ;; one leading space per \n in message
(fn-name arg ...)                     ;; backtrace frame  (hidden by mode)
(fn-name ...)                         ;; unevaluated frame (hidden by mode)
```

A new entry begins at each `[LVL]` line at column 0.



## Keys

| Key | Action |
|--|--|
| `<tab>` | Toggle compact `caller <- ... <- outer` chain for entry at point |
| `b` | Open detailed backtrace in a side window |
| `c` | Hide all current entries without erasing (toggle) |
| `e` | Erase buffer |
| `f` | Filter: show only entries matching a regexp |
| `s` | Snapshot visible entries to a timestamped `.log` file |
| `l1`-`l4` | Set minimum display level (1=debug 2=info 3=warn 4=error) |

The header line always shows `visible/total`, active filter, and level cutoff.

Function names in the compact chain, caller tag, and detailed backtrace are
all clickable (`mouse-1` / `RET`) and jump to the function's definition via
a single shared keymap.



## `%=` format specifier

`%=SPEC` expands to `label=value`.  The label is derived from the unevaluated
argument form (symbol name, or `prin1` for complex expressions). All other
format specifiers work as expected.

```elisp
(let ((buf "foo.el") (line 10))
  (dmsg "at %=s %=d" buf line))
;; at buf=foo.el line=10
```


## Visibility and filtering

All filtering hides entries via overlays.

- **`f`**  regexp filter on message text.
- **`c`**  hide all current entries. Press `c` again to restore.
- **`l 1` -- `l 4`**  set minimum shown level.
- **`dmsg-max-entries`** hides oldest entries beyond this count (`nil` = unlimited).



## Snapshot

`s` (`dmsg-snapshot`) prompts for a filename, defaulting to a timestamped
`dmsg-YYYYMMDD-HHMMSS.log` in the buffer's `default-directory`.  Navigating
to a different directory without specifying a name saves the file there under
the default name.  Only currently visible entries are written; the buffer is
not modified.


## Intercept `message`

```elisp
;; Route matching message() calls into the dmsg buffer at debug level
(dmsg-on-message "error\\|warning")
(dmsg-on-message nil)  ; disable
```

## Log errors from a function

```elisp
(dmsg-on-error 'my-function)          ; add (or toggle)
```

When active, errors from the function are logged at `error` level and
re-signalled normally, existing error handling is otherwise unaffected.
