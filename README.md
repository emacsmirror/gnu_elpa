# Minibuffer Confines Transcended (mct.el)

[ **UPDATE 2026-09-03:** Starting with Emacs 31, this package is no
  longer needed. You can get almost the same experience by configuring
  built-in features. I did a video on the matter:
  <https://protesilaos.com/codelog/2026-07-29-emacs-default-minibuffer-completion-overview/>. ]

Opinionated changes and enhancements to the default minibuffer
completion UI of Emacs:

- Live completions to update the results as you type.

- Passlist and blocklist of commands or completion categories to
  control whether the `*Completions*` buffer shows up eagerly.

- Other relevant options to control when the `*Completions*` are
  displayed.

- Per command or completion category sorting methods.

- A cleaner `*Completions*` buffer, optionally without a mode line.

- Commands to cycle from the minibuffer to the `*Completions*` and
  vice versa.

In essence, MCT is (i) a layer of interactivity on top of the
out-of-the-box completion experience, and (ii) glue code that
combines built-in functionalities to make the default completion
framework work like that of more featureful third-party options.

+ Package name (GNU ELPA): `mct`
+ Official manual: <https://protesilaos.com/emacs/mct>
+ Change log: <https://protesilaos.com/emacs/mct-changelog>
+ Git repositories:
  + GitHub: <https://github.com/protesilaos/mct>
  + GitLab: <https://gitlab.com/protesilaos/mct>
+ Video demo: <https://protesilaos.com/codelog/2021-10-22-emacs-mct-demo/>
+ Backronym: Minibuffer Confines Transcended; Minibuffer and
  Completions in Tandem.
