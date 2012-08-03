;;; ampc.el --- Asynchronous Music Player Controller -*- lexical-binding: t -*-

;; Copyright (C) 2011-2012 Free Software Foundation, Inc.

;; Author: Christopher Schmidt <christopher@ch.ristopher.com>
;; Maintainer: Christopher Schmidt <christopher@ch.ristopher.com>
;; Version: 0.1.3
;; Created: 2011-12-06
;; Keywords: ampc, mpc, mpd
;; Compatibility: GNU Emacs: 24.x

;; This file is part of ampc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; * description
;; ampc is a controller for the Music Player Daemon (http://mpd.wikia.com/).

;;; ** installation
;; If you use GNU ELPA, install ampc via M-x package-list-packages RET or
;; (package-install 'ampc).  Otherwise, grab the files in this repository and
;; put the emacs lisp ones somewhere in your load-path or add the directory the
;; files are in to it, e.g.:
;;
;; (add-to-list 'load-path "~/.emacs.d/ampc")
;; (autoload 'ampc "ampc" nil t)
;;
;; Byte-compile ampc (M-x byte-compile-file RET /path/to/ampc.el RET) to improve
;; its performance!

;;; ** usage
;; To invoke ampc call the command `ampc', e.g. via M-x ampc RET.  The first
;; argument to `ampc' is the host, the second is the port.  Both values default
;; to nil.  If nil, ampc will use the value specified in `ampc-default-server',
;; by default localhost:6600.  To make ampc use the full frame rather than the
;; selected window for its window setup, customise `ampc-use-full-frame' to a
;; non-nil value.
;;
;; ampc offers three independent views which expose different parts of the user
;; interface.  The current playlist view, the default view at startup, may be
;; accessed using the `J' key (that is `S-j').  The playlist view may be
;; accessed using the `K' key.  The outputs view may be accessed by pressing
;; `L'.

;;; *** current playlist view
;; The playlist view looks like this:
;;
;; .........................
;; . 1      . 3  . 4  . 5  .
;; ..........    .    .    .
;; . 2      .    .    .    .
;; .        .    .    .    .
;; .        .    .    .    .
;; .        ................
;; .        . 6            .
;; .        .              .
;; .........................
;;
;; Window one exposes basic information about the daemon, such as the current
;; state (stop/play/pause), the song currently playing or the volume.
;;
;; All windows, except the status window, contain a tabular list of items.  Each
;; item may be selected/marked.  There may be multiple selections.
;;
;; To mark an entry, move the point to the entry and press `m' (ampc-mark).  To
;; unmark an entry, press `u' (ampc-unmark).  To unmark all entries, press `U'
;; (ampc-unmark-all).  To toggle marks, press `t' (ampc-toggle-marks).  Pressing
;; `<down-mouse-1>' with the mouse mouse cursor on a list entry will move point
;; to the entry and toggle the mark.  To navigate to the next entry, press `n'
;; (ampc-next-line).  Analogous, pressing `p' (ampc-previous-line) moves the
;; point to the previous entry.
;;
;; Window two shows the current playlist.  The song that is currently played by
;; the daemon, if any, is highlighted.  To delete the selected songs from the
;; playlist, press `d' (ampc-delete).  Pressing `<down-mouse-3>' will move the
;; point to the entry under cursor and delete it from the playlist.  To move the
;; selected songs up, press `<up>' (ampc-up).  Analogous, press `<down>'
;; (ampc-down) to move the selected songs down.  Pressing `<return>'
;; (ampc-play-this) or `<down-mouse-2>' will play the song at point/cursor.
;;
;; Windows three to five are tag browsers.  You use them to narrow the song
;; database to certain songs.  Think of tag browsers as filters, analogous to
;; piping `grep' outputs through additional `grep' filters.  The property of the
;; songs that is filtered is displayed in the header line of the window.
;;
;; Window six shows the songs that match the filters defined by windows three to
;; five.  To add the selected song to the playlist, press `a' (ampc-add).
;; Pressing `<down-mouse-3>' will move the point to the entry under the cursor
;; and execute `ampc-add'.  These key bindings works in tag browsers as well.
;; Calling `ampc-add' in a tag browser adds all songs filtered up to the
;; selected browser to the playlist.
;;
;; The tag browsers of the current playlist view (accessed via `J') are `Genre'
;; (window 3), `Artist' (window 4) and `Album' (window 5).  The key `M' may be
;; used to fire up a slightly modified current playlist view.  There is no
;; difference to the default current playlist view other than that the tag
;; browsers filter to `Genre' (window 3), `Album' (window 4) and `Artist'
;; (window 5).  Metaphorically speaking, the order of the `grep' filters defined
;; by the tag browsers is different.

;;; *** playlist view
;; The playlist view resembles the current playlist view.  The window, which
;; exposes the playlist content, is split, though.  The bottom half shows a list
;; of stored playlists.  The upper half does not expose the current playlist
;; anymore.  Instead, the content of the selected (stored) playlist is shown.
;; All commands that used to work in the current playlist view and modify the
;; current playlist now modify the selected (stored) playlist.  The list of
;; stored playlists is the only view in ampc that may have only one marked
;; entry.
;;
;; To queue a playlist, press `l' (ampc-load) or `<down-mouse-2>'.  To delete a
;; playlist, press `d' (ampc-delete-playlist) or `<down-mouse-3>'.  The command
;; `ampc-rename-playlist', bound to `r', can be used to rename a playlist.
;;
;; Again, the key `<' may be used to setup a playlist view with a different
;; order of tag browsers.

;;; *** outputs view
;; The outputs view contains a single list which shows the configured outputs of
;; MPD.  To toggle the enabled property of the selected outputs, press `a'
;; (ampc-toggle-output-enabled) or `<mouse-3>'.

;;; *** global keys
;; Aside from `J', `M', `K', `<' and `L', which may be used to select different
;; views, ampc defines the following global keys, which may be used in every
;; window associated with ampc:
;;
;; `k' (ampc-toggle-play): Toggle play state.  If MPD does not play a song,
;; start playing the song at point if the current buffer is the playlist buffer,
;; otherwise start at the beginning of the playlist.  With numeric prefix
;; argument 4, stop player rather than pause if applicable.
;;
;; `l' (ampc-next): Play next song.
;; `j' (ampc-previous): Play previous song
;;
;; `c' (ampc-clear): Clear playlist.
;; `s' (ampc-shuffle): Shuffle playlist.
;;
;; `S' (ampc-store): Store playlist.
;; `O' (ampc-load): Load selected playlist into the current playlist.
;; `R' (ampc-rename-playlist): Rename selected playlist.
;; `D' (ampc-delete-playlist): Delete selected playlist.
;;
;; `y' (ampc-increase-volume): Increase volume.
;; `M-y' (ampc-decrease-volume): Decrease volume.
;; `C-M-y' (ampc-set-volume): Set volume.
;; `h' (ampc-increase-crossfade): Increase crossfade.
;; `M-h' (ampc-decrease-crossfade): Decrease crossfade.
;; `C-M-h' (ampc-set-crossfade): Set crossfade.
;;
;; `e' (ampc-toggle-repeat): Toggle repeat state.
;; `r' (ampc-toggle-random): Toggle random state.
;; `f' (ampc-toggle-consume): Toggle consume state.
;;
;; `P' (ampc-goto-current-song): Select the current playlist window and move
;; point to the current song.
;; `G' (ampc-mini): Select song to play via `completing-read'.
;;
;; `T' (ampc-trigger-update): Trigger a database update.
;; `Z' (ampc-suspend): Suspend ampc.
;; `q' (ampc-quit): Quit ampc.
;;
;; The keymap of ampc is designed to fit the QWERTY United States keyboard
;; layout.  If you use another keyboard layout, feel free to modify
;; `ampc-mode-map'.  For example, I use a regular QWERTZ German keyboard
;; (layout), so I modify `ampc-mode-map' in my init.el like this:
;;
;; (eval-after-load 'ampc
;;   '(flet ((substitute-ampc-key
;;            (from to)
;;            (define-key ampc-mode-map to (lookup-key ampc-mode-map from))
;;            (define-key ampc-mode-map from nil)))
;;      (substitute-ampc-key (kbd "z") (kbd "Z"))
;;      (substitute-ampc-key (kbd "y") (kbd "z"))
;;      (substitute-ampc-key (kbd "M-y") (kbd "M-z"))
;;      (substitute-ampc-key (kbd "C-M-y") (kbd "C-M-z"))
;;      (substitute-ampc-key (kbd "<") (kbd ";"))))
;;
;; If ampc is suspended, you can still use every interactive command that does
;; not directly operate on or with the user interace of ampc.  For example it is
;; perfectly fine to call `ampc-increase-volume' or `ampc-toggle-play' via M-x
;; RET.  Especially the commands `ampc-status' and `ampc-mini' are predesignated
;; to be bound in the global keymap and called when ampc is suspended.
;; `ampc-status' messages the information that is displayed by the status window
;; of ampc.  `ampc-mini' lets you select a song to play via `completing-read'.
;; To start ampc suspended, call `ampc' with the third argument being non-nil.
;; To check whether ampc is connected to the daemon and/or suspended, call
;; `ampc-is-on-p' or `ampc-suspended-p'.
;;
;; (global-set-key (kbd "<f7>")
;;                 (lambda ()
;;                   (interactive)
;;                   (unless (ampc-on-p)
;;                     (ampc nil nil t))
;;                   (ampc-status)))
;; (global-set-key (kbd "<f8>")
;;                 (lambda ()
;;                   (interactive)
;;                   (unless (ampc-on-p)
;;                     (ampc nil nil t))
;;                   (ampc-mini)))

;;; Code:
;;; * code
(eval-when-compile
  (require 'cl))
(require 'network-stream)
(require 'avl-tree)

;;; ** declarations
(defgroup ampc ()
  "Asynchronous client for the Music Player Daemon."
  :prefix "ampc-"
  :group 'multimedia
  :group 'applications)

;;; *** customs
(defcustom ampc-debug nil
  "Non-nil means log outgoing communication between ampc and MPD.
If the value is neither t nor nil, also log incoming data."
  :type '(choice (const :tag "Disable" nil)
                 (const :tag "Outgoing" t)
                 (const :tag "Incoming and outgoing" full)))

(defcustom ampc-use-full-frame nil
  "If non-nil, ampc will use the entire Emacs screen."
  :type 'boolean)

(defcustom ampc-truncate-lines t
  "If non-nil, truncate lines in ampc buffers."
  :type 'boolean)

(defcustom ampc-default-server '("localhost" . 6600)
  "The MPD server to connect to if the arguments to `ampc' are nil.
This variable is a cons cell, with the car specifying the
hostname and the cdr specifying the port.  Both values can be
nil, which will make ampc query the user for values on each
invocation."
  :type '(cons (choice :tag "Hostname"
                       (string)
                       (const :tag "Ask" nil))
               (choice :tag "Port"
                       (string)
                       (integer)
                       (const :tag "Ask" nil))))

(defcustom ampc-synchronous-commands '(t status currentsong play)
  "List of MPD commands that should be executed synchronously.
Executing commands that print lots of output synchronously will
result in massive performance improvements of ampc.  If the car
of this list is `t', execute all commands synchronously other
than the ones specified by the rest of the list."
  :type '(repeat symbol))

(defcustom ampc-status-tags nil
  "List of additional tags of the current song that are added to
the internal status of ampc and thus are passed to the functions
in `ampc-status-changed-hook'.  Each element may be a string that
specifies a tag that is returned by MPD's `currentsong'
command."
  :type '(list symbol))

(defcustom ampc-volume-step 5
  "Default step of `ampc-increase-volume' and
`ampc-decrease-volume' for changing the volume."
  :type 'integer)

(defcustom ampc-crossfade-step 5
  "Default step of `ampc-increase-crossfade' and
`ampc-decrease-crossfade' for changing the crossfade."
  :type 'integer)

;;; **** hooks
(defcustom ampc-before-startup-hook nil
  "A hook run before startup.
This hook is called as the first thing when ampc is started."
  :type 'hook)

(defcustom ampc-connected-hook nil
  "A hook run after ampc connected to MPD."
  :type 'hook)

(defcustom ampc-suspend-hook nil
  "A hook run when suspending ampc."
  :type 'hook)

(defcustom ampc-quit-hook nil
  "A hook run when exiting ampc."
  :type 'hook)

(defcustom ampc-status-changed-hook nil
  "A hook run whenever the status of the daemon (that is volatile
properties such as volume or current song) changes.  The hook is
run with one arg, an alist that contains the new status.  The car
of each entry is a symbol, the cdr is a string.  Valid keys are:

    volume
    repeat
    random
    consume
    xfade
    state
    song
    Artist
    Title

and the keys in `ampc-status-tags'.  Not all keys may be present
all the time!"
  :type 'hook)

;;; *** faces
(defface ampc-mark-face '((t (:inherit font-lock-constant-face)))
  "Face of the mark.")
(defface ampc-marked-face '((t (:inherit warning)))
  "Face of marked entries.")
(defface ampc-unmarked-face '((t (:inerhit default)))
  "Face of unmarked entries.")
(defface ampc-current-song-mark-face '((t (:inherit region)))
  "Face of mark of the current song.")
(defface ampc-current-song-marked-face '((t (:inherit region)))
  "Face of the current song if marked.")

;;; *** internal variables
(defvar ampc-views
  (let* ((songs '(1.0 song :properties (("Track" :title "#" :width 4)
                                        ("Title" :min 15 :max 40)
                                        ("Time" :width 6)
                                        ("Artist" :min 15 :max 40)
                                        ("Album" :min 15 :max 40))))
         (rs_a `(1.0 vertical
                     (0.7 horizontal
                          (0.33 tag :tag "Genre" :id 1 :select t)
                          (0.33 tag :tag "Artist" :id 2)
                          (1.0 tag :tag "Album" :id 3))
                     ,songs))
         (rs_b `(1.0 vertical
                     (0.7 horizontal
                          (0.33 tag :tag "Genre" :id 1 :select t)
                          (0.33 tag :tag "Album" :id 2)
                          (1.0 tag :tag "Artist" :id 3))
                     ,songs))
         (pl-prop '(:properties (("Title" :min 15 :max 40)
                                 ("Artist" :min 15 :max 40)
                                 ("Album" :min 15 :max 40)
                                 ("Time" :width 6)))))
    `(("Current playlist view (Genre|Artist|Album)"
       ,(kbd "J")
       horizontal
       (0.4 vertical
            (6 status)
            (1.0 current-playlist ,@pl-prop))
       ,rs_a)
      ("Current playlist view (Genre|Album|Artist)"
       ,(kbd "M")
       horizontal
       (0.4 vertical
            (6 status)
            (1.0 current-playlist ,@pl-prop))
       ,rs_b)
      ("Playlist view (Genre|Artist|Album)"
       ,(kbd "K")
       horizontal
       (0.4 vertical
            (6 status)
            (1.0 vertical
                 (0.8 playlist ,@pl-prop)
                 (1.0 playlists)))
       ,rs_a)
      ("Playlist view (Genre|Album|Artist)"
       ,(kbd "<")
       horizontal
       (0.4 vertical
            (6 status)
            (1.0 vertical
                 (0.8 playlist ,@pl-prop)
                 (1.0 playlists)))
       ,rs_b)
      ("Outputs view"
       ,(kbd "L")
       outputs :properties (("outputname" :title "Name" :min 10 :max 30)
                            ("outputenabled" :title "Enabled" :width 9))))))

(defvar ampc-connection nil)
(defvar ampc-host nil)
(defvar ampc-port nil)
(defvar ampc-outstanding-commands nil)

(defvar ampc-no-implicit-next-dispatch nil)
(defvar ampc-working-timer nil)
(defvar ampc-yield nil)
(defvar ampc-yield-redisplay nil)

(defvar ampc-windows nil)
(defvar ampc-all-buffers nil)

(defvar ampc-type nil)
(make-variable-buffer-local 'ampc-type)
(defvar ampc-dirty nil)
(make-variable-buffer-local 'ampc-dirty)

(defvar ampc-internal-db nil)
(defvar ampc-status nil)

;;; *** mode maps
(defvar ampc-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "k") 'ampc-toggle-play)
    (define-key map (kbd "l") 'ampc-next)
    (define-key map (kbd "j") 'ampc-previous)
    (define-key map (kbd "c") 'ampc-clear)
    (define-key map (kbd "s") 'ampc-shuffle)
    (define-key map (kbd "S") 'ampc-store)
    (define-key map (kbd "O") 'ampc-load)
    (define-key map (kbd "R") 'ampc-rename-playlist)
    (define-key map (kbd "D") 'ampc-delete-playlist)
    (define-key map (kbd "y") 'ampc-increase-volume)
    (define-key map (kbd "M-y") 'ampc-decrease-volume)
    (define-key map (kbd "C-M-y") 'ampc-set-volume)
    (define-key map (kbd "h") 'ampc-increase-crossfade)
    (define-key map (kbd "M-h") 'ampc-decrease-crossfade)
    (define-key map (kbd "C-M-h") 'ampc-set-crossfade)
    (define-key map (kbd "e") 'ampc-toggle-repeat)
    (define-key map (kbd "r") 'ampc-toggle-random)
    (define-key map (kbd "f") 'ampc-toggle-consume)
    (define-key map (kbd "P") 'ampc-goto-current-song)
    (define-key map (kbd "G") 'ampc-mini)
    (define-key map (kbd "q") 'ampc-quit)
    (define-key map (kbd "z") 'ampc-suspend)
    (define-key map (kbd "T") 'ampc-trigger-update)
    (loop for view in ampc-views
          do (define-key map (cadr view)
               `(lambda ()
                  (interactive)
                  (ampc-change-view ',view))))
    map))

(defvar ampc-item-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "m") 'ampc-mark)
    (define-key map (kbd "u") 'ampc-unmark)
    (define-key map (kbd "U") 'ampc-unmark-all)
    (define-key map (kbd "n") 'ampc-next-line)
    (define-key map (kbd "p") 'ampc-previous-line)
    (define-key map (kbd "<down-mouse-1>") 'ampc-mouse-toggle-mark)
    (define-key map (kbd "<mouse-1>") 'ampc-mouse-align-point)
    (define-key map [remap next-line] 'ampc-next-line)
    (define-key map [remap previous-line] 'ampc-previous-line)
    (define-key map [remap tab-to-tab-stop] 'ampc-move-to-tab)
    map))

(defvar ampc-current-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "<return>") 'ampc-play-this)
    (define-key map (kbd "<down-mouse-2>") 'ampc-mouse-play-this)
    (define-key map (kbd "<mouse-2>") 'ampc-mouse-align-point)
    (define-key map (kbd "<down-mouse-3>") 'ampc-mouse-delete)
    (define-key map (kbd "<mouse-3>") 'ampc-mouse-align-point)
    map))

(defvar ampc-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "t") 'ampc-toggle-marks)
    (define-key map (kbd "d") 'ampc-delete)
    (define-key map (kbd "<up>") 'ampc-up)
    (define-key map (kbd "<down>") 'ampc-down)
    (define-key map (kbd "<down-mouse-3>") 'ampc-mouse-delete)
    (define-key map (kbd "<mouse-3>") 'ampc-mouse-align-point)
    map))

(defvar ampc-playlists-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "l") 'ampc-load)
    (define-key map (kbd "r") 'ampc-rename-playlist)
    (define-key map (kbd "d") 'ampc-delete-playlist)
    (define-key map (kbd "<down-mouse-2>") 'ampc-mouse-load)
    (define-key map (kbd "<mouse-2>") 'ampc-mouse-align-point)
    (define-key map (kbd "<down-mouse-3>") 'ampc-mouse-delete-playlist)
    (define-key map (kbd "<mouse-3>") 'ampc-mouse-align-point)
    map))

(defvar ampc-tag-song-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "t") 'ampc-toggle-marks)
    (define-key map (kbd "a") 'ampc-add)
    (define-key map (kbd "<down-mouse-3>") 'ampc-mouse-add)
    (define-key map (kbd "<mouse-3>") 'ampc-mouse-align-point)
    map))

(defvar ampc-outputs-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "t") 'ampc-toggle-marks)
    (define-key map (kbd "a") 'ampc-toggle-output-enabled)
    (define-key map (kbd "<down-mouse-3>") 'ampc-mouse-toggle-output-enabled)
    (define-key map (kbd "<mouse-3>") 'ampc-mouse-align-point)
    map))

;;; **** menu
(easy-menu-define nil ampc-mode-map nil
  `("ampc"
    ("Change view" ,@(loop for view in ampc-views
                           collect (vector (car view)
                                           `(lambda ()
                                              (interactive)
                                              (ampc-change-view ',view)))))
    "--"
    ["Play" ampc-toggle-play
     :visible (and ampc-status
                   (not (equal (cdr (assq 'state ampc-status)) "play")))]
    ["Pause" ampc-toggle-play
     :visible (and ampc-status
                   (equal (cdr (assq 'state ampc-status)) "play"))]
    ["Stop" (lambda () (interactive) (ampc-toggle-play 4))
     :visible (and ampc-status
                   (equal (cdr (assq 'state ampc-status)) "play"))]
    ["Next" ampc-next]
    ["Previous" ampc-previous]
    "--"
    ["Clear playlist" ampc-clear]
    ["Shuffle playlist" ampc-shuffle]
    ["Store playlist" ampc-store]
    ["Queue Playlist" ampc-load :visible (ampc-playlist)]
    ["Rename Playlist" ampc-rename-playlist :visible (ampc-playlist)]
    ["Delete Playlist" ampc-delete-playlist :visible (ampc-playlist)]
    "--"
    ["Increase volume" ampc-increase-volume]
    ["Decrease volume" ampc-decrease-volume]
    ["Set volume" ampc-set-volume]
    ["Increase crossfade" ampc-increase-crossfade]
    ["Decrease crossfade" ampc-decrease-crossfade]
    ["Set crossfade" ampc-set-crossfade]
    ["Toggle repeat" ampc-toggle-repeat
     :style toggle
     :selected (equal (cdr (assq 'repeat ampc-status)) "1")]
    ["Toggle random" ampc-toggle-random
     :style toggle
     :selected (equal (cdr (assq 'random ampc-status)) "1")]
    ["Toggle consume" ampc-toggle-consume
     :style toggle
     :selected (equal (cdr (assq 'consume ampc-status)) "1")]
    "--"
    ["Trigger update" ampc-trigger-update]
    ["Suspend" ampc-suspend]
    ["Quit" ampc-quit]))

(easy-menu-define ampc-selection-menu ampc-item-mode-map
  "Selection menu for ampc"
  '("ampc Mark"
    ["Add to playlist" ampc-add
     :visible (not (eq (car ampc-type) 'outputs))]
    ["Toggle enabled" ampc-toggle-output-enabled
     :visible (eq (car ampc-type) 'outputs)]
    "--"
    ["Next line" ampc-next-line]
    ["Previous line" ampc-previous-line]
    ["Mark" ampc-mark]
    ["Unmark" ampc-unmark]
    ["Unmark all" ampc-unmark-all]
    ["Toggle marks" ampc-toggle-marks
     :visible (not (eq (car ampc-type) 'playlists))]))

(defvar ampc-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item
     "mpc/prev" 'ampc-previous 'previous map
     :help "Previous")
    (tool-bar-local-item
     "mpc/play" 'ampc-toggle-play 'play map
     :help "Play"
     :visible '(and ampc-status
                    (not (equal (cdr (assq 'state ampc-status)) "play"))))
    (tool-bar-local-item
     "mpc/pause" 'ampc-toggle-play 'pause map
     :help "Pause"
     :visible '(and ampc-status
                    (equal (cdr (assq 'state ampc-status)) "play")))
    (tool-bar-local-item
     "mpc/stop" (lambda () (interactive) (ampc-toggle-play 4)) 'stop map
     :help "Stop"
     :visible '(and ampc-status
                    (equal (cdr (assq 'state ampc-status)) "play")))
    (tool-bar-local-item
     "mpc/next" 'ampc-next 'next map
     :help "Next")
    map))

;;; ** code
;;; *** macros
(defmacro ampc-with-buffer (type &rest body)
  (declare (indent 1) (debug t))
  `(let* ((type- ,type)
          (w (if (windowp type-)
                 type-
               (loop for w in (ampc-normalize-windows)
                     thereis (when (with-current-buffer
                                       (window-buffer w)
                                     (etypecase type-
                                       (symbol (eq (car ampc-type) type-))
                                       (cons (equal ampc-type type-))))
                               w)))))
     (when w
       (with-selected-window w
         (with-current-buffer (window-buffer w)
           (let ((inhibit-read-only t))
             ,@(if (eq (car body) 'no-se)
                   (cdr body)
                 `((save-excursion
                     (goto-char (point-min))
                     ,@body)))))))))

(defmacro ampc-fill-skeleton (tag &rest body)
  (declare (indent 1) (debug t))
  `(let ((tag- ,tag)
         (data-buffer (current-buffer)))
     (ampc-with-buffer tag-
       no-se
       (let ((point (point)))
         (goto-char (point-min))
         (loop until (eobp)
               do (put-text-property (point) (1+ (point)) 'updated t)
               (forward-line))
         (goto-char (point-min))
         ,@body
         (goto-char (point-min))
         (loop until (eobp)
               when (get-text-property (point) 'updated)
               do (delete-region (point) (1+ (line-end-position)))
               else
               do (add-text-properties
                   (+ (point) 2)
                   (progn (forward-line nil)
                          (1- (point)))
                   '(mouse-face highlight))
               end)
         (goto-char point)
         (ampc-align-point))
       (ampc-set-dirty nil)
       (with-selected-window (if (windowp tag-) tag- (ampc-get-window tag-))
         (recenter)))))

(defmacro ampc-with-selection (arg &rest body)
  (declare (indent 1) (debug t))
  `(let ((arg- ,arg))
     (if (or (and (not arg-)
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward-regexp "^* " nil t)))
             (and arg- (symbolp arg-)))
         (loop initially (goto-char (point-min))
               finally (ampc-align-point)
               while (search-forward-regexp "^* " nil t)
               for index from 0
               do (save-excursion
                    ,@body))
       (loop until (eobp)
             for index from 0 to (1- (if (numberp arg-)
                                         arg-
                                       (prefix-numeric-value arg-)))
             do (save-excursion
                  (goto-char (line-end-position))
                  ,@body)
             until (ampc-next-line)))))

;;; *** modes
(define-derived-mode ampc-outputs-mode ampc-item-mode "ampc-o")

(define-derived-mode ampc-tag-song-mode ampc-item-mode "ampc-ts")

(define-derived-mode ampc-current-playlist-mode ampc-playlist-mode "ampc-cpl"
  (ampc-highlight-current-song-mode))

(define-derived-mode ampc-playlist-mode ampc-item-mode "ampc-pl")

(define-derived-mode ampc-playlists-mode ampc-item-mode "ampc-pls")


  (set (make-local-variable 'tool-bar-map) ampc-tool-bar-map)
  (setf truncate-lines ampc-truncate-lines
        font-lock-defaults '((("^\\(\\*\\)\\(.*\\)$"
(define-derived-mode ampc-item-mode ampc-mode "ampc-item"
  (setf font-lock-defaults '((("^\\(\\*\\)\\(.*\\)$"
                               (1 'ampc-mark-face)
                               (2 'ampc-marked-face))
                              ("" 0 'ampc-unmarked-face))
                             t)))

(define-derived-mode ampc-mode special-mode "ampc"
  (buffer-disable-undo)
  (set (make-local-variable 'tool-bar-map) ampc-tool-bar-map)
  (setf truncate-lines ampc-truncate-lines
        mode-line-modified "--"))

(define-minor-mode ampc-highlight-current-song-mode ""
  nil
  nil
  nil
  (funcall (if ampc-highlight-current-song-mode
               'font-lock-add-keywords
             'font-lock-remove-keywords)
           nil
           '((ampc-find-current-song
              (1 'ampc-current-song-mark-face)
              (2 'ampc-current-song-marked-face)))))

;;; *** internal functions
(defun ampc-normalize-windows ()
  (setf ampc-windows
        (loop for (window . buffer) in ampc-windows
              collect (cons (if (and (window-live-p window)
                                     (eq (window-buffer window) buffer))
                                window
                              (get-buffer-window buffer))
                            buffer)))
  (delq nil (mapcar 'car ampc-windows)))

(defun ampc-restore-window-configuration ()
  (let ((windows
          (sort (delq nil
                      (mapcar (lambda (w)
                                (when (eq (window-frame w)
                                          (selected-frame))
                                  w))
                              (ampc-normalize-windows)))
                (lambda (w1 w2)
                  (loop for w in (window-list nil nil (frame-first-window))
                        do (when (eq w w1)
                             (return t))
                        (when (eq w w2)
                          (return nil)))))))
    (when windows
      (setf (window-dedicated-p (car windows)) nil)
      (loop for w in (cdr windows)
            do (delete-window w)))))

(defun ampc-change-view (view)
  (if (equal ampc-outstanding-commands '((idle)))
      (ampc-configure-frame (cddr view))
    (message "ampc is busy, cannot change window layout")))

(defun ampc-quote (string)
  (concat "\"" (replace-regexp-in-string "\"" "\\\"" string) "\""))

(defun ampc-in-ampc-p ()
  (when (ampc-on-p)
    ampc-type))

(defun ampc-add-impl (&optional data)
  (cond ((null data)
         (loop for d in (get-text-property (line-end-position) 'data)
               do (ampc-add-impl d)))
        ((avl-tree-p data)
         (avl-tree-mapc (lambda (e) (ampc-add-impl (cdr e))) data))
        ((stringp data)
         (if (ampc-playlist)
             (ampc-send-command 'playlistadd
                                t
                                (ampc-quote (ampc-playlist))
                                data)
           (ampc-send-command 'add t (ampc-quote data))))
        (t
         (loop for d in (reverse data)
               do (ampc-add-impl (cdr (assoc "file" d)))))))

(defun ampc-skip (N)
  (ampc-send-command 'play
                     nil
                     (let ((N N))
                       (lambda ()
                         (let ((song (cdr-safe (assq 'song ampc-status))))
                           (unless song
                             (throw 'skip nil))
                           (max 0 (+ (string-to-number song) N))))))
  (ampc-send-command 'status t))

(defun* ampc-find-current-song
    (limit &aux (point (point)) (song (cdr (assq 'song ampc-status))))
  (when (and song
             (<= (1- (line-number-at-pos (point)))
                 (setf song (string-to-number song)))
             (>= (1- (line-number-at-pos limit)) song))
    (goto-char (point-min))
    (forward-line song)
    (save-restriction
      (narrow-to-region (max point (point)) (min limit (line-end-position)))
      (search-forward-regexp "\\(?1:\\(\\`\\*\\)?\\)\\(?2:.*\\)$"))))

(defun ampc-set-volume-impl (arg &optional func)
  (when arg
    (setf arg (prefix-numeric-value arg)))
  (ampc-send-command
   'setvol
   t
   (lambda ()
     (unless ampc-status
       (throw 'skip nil))
     (max (min (if func
                   (funcall func
                            (string-to-number
                             (cdr (assq 'volume ampc-status)))
                            (or arg ampc-volume-step))
                 arg)
               100)
          0)))
  (ampc-send-command 'status t))

(defun ampc-set-crossfade-impl (arg &optional func)
(defun* ampc-fix-pos (f &aux buffer-read-only)
  (save-excursion
    (move-beginning-of-line nil)
    (let* ((data (get-text-property (+ 2 (point)) 'data))
           (pos (assoc "Pos" data)))
      (setf (cdr pos) (funcall f (cdr pos)))
      (put-text-property (+ 2 (point))
                         (line-end-position)
                         'data
                         data))))

(defun* ampc-move-impl (up &aux (line (1- (line-number-at-pos))))
  (when (or (and up (eq line 0))
            (and (not up) (eq (1+ line) (line-number-at-pos (1- (point-max))))))
    (return-from ampc-move-impl t))
  (when arg
    (setf arg (prefix-numeric-value arg)))
  (ampc-send-command
   'crossfade
   t
   (lambda ()
     (unless ampc-status
       (throw 'skip nil))
     (max (if func
              (funcall func
                       (string-to-number
                        (cdr (assq 'xfade ampc-status)))
                       (or arg ampc-crossfade-step))
            arg)
          0)))
  (ampc-send-command 'status t))

  (save-excursion
    (move-beginning-of-line nil)
    (if (ampc-playlist)
        (ampc-send-command 'playlistmove
                           nil
                           (ampc-quote (ampc-playlist))
                           line
                           (funcall (if up '1- '1+)
                                    line))
      (ampc-send-command 'move nil line (funcall (if up '1- '1+) line)))
    (unless up
      (forward-line))
    (unless (ampc-playlist)
      (save-excursion
        (forward-line -1)
        (ampc-fix-pos '1+))
      (ampc-fix-pos '1-))
    (let ((buffer-read-only))
      (transpose-lines 1)))
  (if up
      (ampc-align-point)
    (ampc-next-line))
  nil)

(defun* ampc-move (up N &aux (point (point)))
  (goto-char (if up (point-min) (point-max)))
  (if (and (not N)
           (funcall (if up 'search-forward-regexp 'search-backward-regexp)
                    "^* "
                    nil
                    t))
      (loop until (ampc-move-impl up)
            unless up
            do (search-backward-regexp "^* " nil t)
            end
            until (not (funcall (if up
                                    'search-forward-regexp
                                  'search-backward-regexp)
                                "^* "
                                nil
                                t))
            finally (unless up
                      (forward-char 2)))
    (goto-char point)
    (unless (eobp)
      (unless N
        (setf N 1))
      (unless up
        (unless (eq (1- N) 0)
          (setf N (- (- (forward-line (1- N)) (1- N))))))
      (loop repeat N
            until (ampc-move-impl up)))))

(defun ampc-toggle-state (state arg)
  (when (or arg ampc-status)
    (ampc-send-command
     state
     nil
     (cond ((null arg)
            (if (equal (cdr (assq state ampc-status)) "1")
                0
              1))
           ((> (prefix-numeric-value arg) 0) 1)
           (t 0)))))

(defun ampc-playlist (&optional at-point)
  (ampc-with-buffer 'playlists
    (if (and (not at-point)
             (search-forward-regexp "^* \\(.*\\)$" nil t))
        (let ((result (match-string 1)))
          (set-text-properties 0 (length result) nil result)
          result)
      (unless (eobp)
        (buffer-substring-no-properties
         (+ (line-beginning-position) 2)
         (line-end-position))))))

(defun* ampc-mark-impl (select N &aux result (inhibit-read-only t))
  (when (eq (car ampc-type) 'playlists)
    (assert (or (not select) (null N) (eq N 1)))
    (ampc-with-buffer 'playlists
      (loop while (search-forward-regexp "^\\* " nil t)
            do (replace-match "  " nil nil))))
  (loop repeat (or N 1)
        until (eobp)
        do (move-beginning-of-line nil)
        (delete-char 1)
        (insert (if select "*" " "))
        (setf result (ampc-next-line nil)))
  (ampc-post-mark-change-update)
  result)

(defun ampc-post-mark-change-update ()
  (ecase (car ampc-type)
    ((current-playlist playlist outputs))
    (playlists
     (ampc-update-playlist))
    ((song tag)
     (loop
      for w in
      (loop for w on (ampc-normalize-windows)
            thereis (when (or (eq (car w) (selected-window))
                              (and (eq (car ampc-type) 'tag)
                                   (eq (with-current-buffer
                                           (window-buffer (car w))
                                         (car ampc-type))
                                       'song)))
                      (cdr w)))
      do (with-current-buffer (window-buffer w)
           (when (memq (car ampc-type) '(song tag))
             (ampc-set-dirty t))))
     (ampc-fill-tag-song))

(defun ampc-align-point ()
  (unless (eobp)
    (move-beginning-of-line nil)
    (forward-char 2)
    (re-search-forward " *" nil t)))

(defun* ampc-pad (tabs &optional dont-honour-item-mode)
  (loop with new-tab-stop-list
        with offset-dec = (if (and (not dont-honour-item-mode)
                                   (derived-mode-p 'ampc-item-mode))
                              2
                            0)
        for tab in tabs
        for offset-cell on (if (derived-mode-p 'ampc-item-mode)
                               tab-stop-list
                             (cons 0 tab-stop-list))
        for offset = (car offset-cell)
        for props in (or (plist-get (cdr ampc-type) :properties)
                         '(nil . nil))
        by (lambda (cell) (or (cdr cell) '(nil . nil)))
        do (decf offset offset-dec)
        with first = t
        with current-offset = 0
        when (<= current-offset offset)
        do (when (and (not first) (eq (- offset current-offset) 0))
             (incf offset))
        and concat (make-string (- offset current-offset) ? ) into result
        and do (setf current-offset offset)
        else
        concat " " into result
        and do (incf current-offset)
        end
        do (unless tab
             (setf tab ""))
        (when (and (plist-get (cdr props) :shrink)
                   (cadr offset-cell)
                   (>= (+ current-offset (length tab) 1) (- (cadr offset-cell)
                                                            offset-dec)))
          (setf tab (concat (substring tab 0 (max (- (cadr offset-cell)
                                                     offset-dec
                                                     current-offset
                                                     4)
                                                  3))
                            "...")))
        concat tab into result
        do (push (+ current-offset offset-dec) new-tab-stop-list)
        (incf current-offset (length tab))
        (setf first nil)
        finally return
        (if (equal (callf nreverse new-tab-stop-list) tab-stop-list)
            result
          (propertize result 'tab-stop-list new-tab-stop-list))))

(defun ampc-update-header ()
  (setf header-line-format
        (unless (eq (car ampc-type) 'status)
          (concat
           (when ampc-dirty
             "  [ Updating... ]")
           (make-string (floor (fringe-columns 'left t)) ? )
           (ecase (car ampc-type)
             (tag
              (concat "  " (plist-get (cdr ampc-type) :tag)))
             (playlists
              "  Playlists")
             (t
              (ampc-pad (loop for (name . props) in
                              (plist-get (cdr ampc-type) :properties)
                              collect (or (plist-get props :title) name))
                        t)))))))

(defun ampc-set-dirty (tag-or-dirty &optional dirty)
  (if (or (null tag-or-dirty) (eq tag-or-dirty t))
      (progn (setf ampc-dirty tag-or-dirty)
             (ampc-update-header))
    (loop for w in (ampc-windows)
          do (with-current-buffer (window-buffer w)
               (when (eq (car ampc-type) tag-or-dirty)
                 (ampc-set-dirty dirty))))))

(defun ampc-update ()
  (if ampc-status
      (loop for w in (ampc-normalize-windows)
            do (with-current-buffer (window-buffer w)
                 (when (and ampc-dirty (not (eq ampc-dirty 'keep-dirty)))
                   (ecase (car ampc-type)
                     (outputs
                      (ampc-send-command 'outputs))
                     (playlist
                      (ampc-update-playlist))
                     ((tag song)
                      (if (assoc (ampc-tags) ampc-internal-db)
                          (ampc-fill-tag-song)
                        (push (cons (ampc-tags) nil) ampc-internal-db)
                        (ampc-send-command 'listallinfo)))
                     (status
                      (ampc-send-command 'status)
                      (ampc-send-command 'currentsong))
                     (playlists
                      (ampc-send-command 'listplaylists))
                     (current-playlist
                      (ampc-send-command 'playlistinfo))))))
    (ampc-send-command 'status)
    (ampc-send-command 'currentsong)))

(defun ampc-update-playlist ()
  (ampc-with-buffer 'playlists
    (if (search-forward-regexp "^\\* " nil t)
        (ampc-send-command 'listplaylistinfo
                           nil
                           (get-text-property (point) 'data))
      (ampc-with-buffer 'playlist
        (erase-buffer)
        (ampc-set-dirty nil)))))

(defun ampc-send-command-impl (command)
  (when ampc-debug
    (message "ampc: -> %s" command))
  (process-send-string ampc-connection (concat command "\n")))

(defun ampc-send-command (command &optional unique &rest args)
  (if (equal command 'idle)
      (when ampc-working-timer
        (cancel-timer ampc-working-timer)
        (setf ampc-yield nil
              ampc-working-timer nil)
        (ampc-fill-status))
    (unless ampc-working-timer
      (setf ampc-yield 0
            ampc-working-timer (run-at-time nil 0.1 'ampc-yield))))
  (setf command (apply 'list command args))
  (when (equal (car-safe ampc-outstanding-commands) '(idle))
    (setf (car ampc-outstanding-commands) '(noidle))
    (ampc-send-command-impl "noidle"))
  (setf ampc-outstanding-commands
        (nconc (if unique
                   ampc-outstanding-commands
                 (delete command ampc-outstanding-commands))
               (list command))))

(defun ampc-send-next-command ()
  (loop while ampc-outstanding-commands
        for command = (replace-regexp-in-string
                       "^.*?-" ""
                       (symbol-name (caar ampc-outstanding-commands)))
        do
        (loop until (catch 'skip
                      (ampc-send-command-impl
                       (concat command
                               (loop for a in (cdar ampc-outstanding-commands)
                                     concat " "
                                     do (when (functionp a)
                                          (setf a (funcall a)))
                                     concat (typecase a
                                              (integer (number-to-string a))
                                              (t a)))))
                      t)
              do (pop ampc-outstanding-commands))
        while (and ampc-outstanding-commands (not (eq (intern command) 'idle)))
        while
        (let ((member (memq (intern command) ampc-synchronous-commands)))
          (when (or (and (not (eq (car ampc-synchronous-commands) t)) member)
                    (and (eq (car ampc-synchronous-commands) t) (not member)))
            (loop with head = ampc-outstanding-commands
                  with ampc-no-implicit-next-dispatch = t
                  with ampc-yield-redisplay = t
                  while (eq head ampc-outstanding-commands)
                  do (accept-process-output ampc-connection 0 100))
            t)))
  (unless ampc-outstanding-commands
    (ampc-send-command 'idle)
    (ampc-send-next-command)))

(defun ampc-tree< (a b)
  (string< (car a) (car b)))

(defun ampc-create-tree ()
  (avl-tree-create 'ampc-tree<))

(defun ampc-extract (tag &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if (listp tag)
        (ampc-extract (plist-get tag :tag))
      (save-excursion
        (goto-char (point-min))
        (when (search-forward-regexp
               (concat "^" (regexp-quote tag) ": \\(.*\\)$")
               nil
               t)
          (let ((result (match-string 1)))
            (when (equal tag "Time")
              (setf result (ampc-transform-time result)))
            result))))))

(defun ampc-insert (element data &optional cmp)
  (save-excursion
    (goto-char (point-min))
    (ecase
        (loop until (eobp)
              for tp = (get-text-property (+ (point) 2) 'data)
              finally return 'insert
              thereis
              (cond ((eq cmp t)
                     (let ((s (buffer-substring-no-properties
                               (+ (point) 2)
                               (line-end-position))))
                       (cond ((equal s element)
                              (unless (member data tp)
                                (put-text-property (+ (point) 2)
                                                   (1+ (line-end-position))
                                                   'data
                                                   `(,data . ,tp)))
                              'update)
                             ((string< element s)
                              'insert))))
                    (cmp
                     (let ((r (funcall cmp data tp)))
                       (if (memq r '(update insert))
                           r
                         (forward-line (1- r))
                         nil)))
                    ((equal tp data)
                     'update)
                    (t
                     (let ((s (buffer-substring-no-properties
                               (+ (point) 2)
                               (line-end-position))))
                       (unless (string< s element)
                         'insert))))
              do (forward-line))
      (insert
       (insert "  ")
       (let ((start (point)))
         (insert element "\n")
         (put-text-property start (point) 'data (if (eq cmp t)
                                                    `(,data)
                                                  data))))
      (update
       (remove-text-properties (point) (1+ (point)) '(updated))
       (equal (buffer-substring (point) (1+ (point))) "*")))))

(defun ampc-fill-tag (trees)
  (put-text-property (point-min) (point-max) 'data nil)
  (loop with new-trees
        for tree in trees
        do (when tree
             (avl-tree-mapc
              (lambda (e)
                (when (ampc-insert (car e) (cdr e) t (car e))
                  (push (cdr e) new-trees)))
              tree))
        finally return new-trees))

(defun ampc-fill-song (trees)
  (loop
   for songs in trees
   do (loop for song in songs
            do (ampc-insert
                (ampc-pad
                 (loop for (p . v) in (plist-get (cdr ampc-type) :properties)
                       collect (cdr (assoc p song))))
                `((,song))))))

(defun* ampc-narrow-entry (&optional (delimiter "file") &aux result)
  (narrow-to-region
   (move-beginning-of-line nil)
   (or (progn (goto-char (line-end-position))
              (when (search-forward-regexp
                     (concat "^" (regexp-quote delimiter) ": ")
                     nil
                     t)
                (move-beginning-of-line nil)
                (setf result (point))
                (1- (point))))
       (point-max)))
  result)

(defun ampc-get-window (type)
  (loop for w in (ampc-windows)
        thereis (with-current-buffer (window-buffer w)
                  (when (eq (car ampc-type) type)
                    w))))

(defun* ampc-fill-playlist (&aux properties)
  (ampc-fill-skeleton 'playlist
    (setf properties (plist-get (cdr ampc-type) :properties))
    (with-current-buffer data-buffer
      (loop
       for i from 0
       with next
       while (or (when next (goto-char next) t)
                 (search-forward-regexp "^file: " nil t))
       do (save-restriction
            (setf next (ampc-narrow-entry))
            (let ((file (ampc-extract "file"))
                  (pad-data (loop for (tag . tag-properties) in properties
                                  collect (or (ampc-extract tag)
                                              "[Not Specified]"))))
              (ampc-with-buffer 'playlist
                (ampc-insert (ampc-pad pad-data 2)
                             `(("file" . ,file)
                               (index . ,i))
                             (lambda (a b)
                               (let ((p1 (cdr (assoc 'index a)))
                                     (p2 (cdr (assoc 'index b))))
                                 (cond ((< p1 p2) 'update)
                                       ((eq p1 p2)
                                        (if (equal (cdr (assoc "file" a))
                                                   (cdr (assoc "file" b)))
                                            'update
                                          'insert))
                                       (t (- p1 p2)))))))))))))

(defun* ampc-fill-outputs (&aux properties)
  (ampc-fill-skeleton 'outputs
    (setf properties (plist-get (cdr ampc-type) :properties))
    (with-current-buffer data-buffer
      (loop
       with next
       while (or (when next (goto-char next) t)
                 (search-forward-regexp "^outputid: " nil t))
       do (save-restriction
            (setf next (ampc-narrow-entry "outputid"))
            (let ((outputid (ampc-extract "outputid"))
                  (outputenabled (ampc-extract "outputenabled")))
              (ampc-with-buffer 'outputs
                (ampc-insert (ampc-pad
                              (loop for (tag . tag-properties) in properties
                                    collect (with-current-buffer data-buffer
                                              (ampc-extract tag)))
                              2)
                             `(("outputid" . ,outputid)
                               ("outputenabled" . ,outputenabled))))))))))

(defun* ampc-mini-impl (&aux songs)
  (loop with next
        while (or (when next (goto-char next) t)
                  (search-forward-regexp "^file: " nil t))
        for entry = (save-restriction
                      (setf next (ampc-narrow-entry))
                      `(,(concat (ampc-extract "Title") " - "
                                 (ampc-extract "Artist"))
                        . ,(string-to-number (ampc-extract "Pos"))))
        do (loop with mentry = `(,(car entry) . ,(cdr entry))
                 for index from 2
                 while (assoc (car mentry) songs)
                 do (setf (car mentry) (concat (car entry)
                                               " (" (int-to-string index) ")"))
                 finally do (push mentry songs)))
  (unless songs
    (message "No song in the playlist")
    (return-from ampc-mini-impl))
  (let ((song (assoc (let ((inhibit-quit t))
                       (prog1
                           (with-local-quit
                             (completing-read "Song to play: " songs nil t))
                         (setf quit-flag nil)))
                     songs)))
    (when song
      (ampc-play-this (cdr song)))))

(defun* ampc-fill-current-playlist (&aux properties)
  (ampc-fill-skeleton 'current-playlist
    (setf properties (plist-get (cdr ampc-type) :properties))
    (with-current-buffer data-buffer
      (loop
       with next
       while (or (when next (goto-char next) t)
                 (search-forward-regexp "^file: " nil t))
       do (save-restriction
            (setf next (ampc-narrow-entry))
            (let ((file (ampc-extract "file"))
                  (pos (ampc-extract "Pos")))
              (ampc-with-buffer 'current-playlist
                (ampc-insert
                 (ampc-pad
                  (loop for (tag . tag-properties) in properties
                        collect (or (with-current-buffer data-buffer
                                      (ampc-extract tag))
                                    "[Not Specified]"))
                  2)
                 `(("file" . ,file)
                   ("Pos" . ,(string-to-number pos)))
                 (lambda (a b)
                   (let ((p1 (cdr (assoc "Pos" a)))
                         (p2 (cdr (assoc "Pos" b))))
                     (cond ((< p1 p2) 'insert)
                           ((eq p1 p2)
                            (if (equal (cdr (assoc "file" a))
                                       (cdr (assoc "file" b)))
                                'update
                              'insert))
                           (t (- p1 p2)))))))))))))

(defun ampc-fill-playlists ()
  (ampc-fill-skeleton 'playlists
    (with-current-buffer data-buffer
      (loop while (search-forward-regexp "^playlist: \\(.*\\)$" nil t)
            for playlist = (match-string 1)
            do (ampc-with-buffer 'playlists
                 (ampc-insert playlist playlist)))))
  (ampc-set-dirty 'playlist t)
  (ampc-update))

(defun ampc-yield ()
  (incf ampc-yield)
  (ampc-fill-status)
  (when ampc-yield-redisplay
    (redisplay t)))

(defun ampc-fill-status ()
  (ampc-with-buffer 'status
    (erase-buffer)
    (funcall (or (plist-get (cadr ampc-type) :filler)
                 (lambda (_)
                   (insert (ampc-status t) "\n")))
             ampc-status)
    (ampc-set-dirty nil)))

(defun ampc-fill-tag-song ()
  (loop
   with trees = `(,(cdr (assoc (ampc-tags) ampc-internal-db)))
   for w in (ampc-windows)
   do
   (ampc-with-buffer w
     (when (member (car ampc-type) '(tag song))
       (if ampc-dirty
           (ampc-fill-skeleton w
             (ecase (car ampc-type)
               (tag (setf trees (ampc-fill-tag trees)))
               (song (ampc-fill-song trees))))
         (setf trees nil)
         (loop while (search-forward-regexp "^* " nil t)
               do (setf trees (append (get-text-property (point) 'data)
                                      trees))))))))

(defun* ampc-transform-time (data &aux (time (string-to-number data)))
  (concat (number-to-string (/ time 60))
          ":"
          (when (< (% time 60) 10)
            "0")
          (number-to-string (% time 60))))

(defun ampc-handle-idle ()
  (loop until (eobp)
        for subsystem = (buffer-substring (point) (line-end-position))
        when (string-match "^changed: \\(.*\\)$" subsystem)
        do (case (intern (match-string 1 subsystem))
             (database
              (setf ampc-internal-db nil)
              (ampc-set-dirty 'tag t)
              (ampc-set-dirty 'song t))
             (output
              (ampc-set-dirty 'outputs t))
             ((player options mixer)
              (setf ampc-status nil)
              (ampc-set-dirty 'status t))
             (stored_playlist
              (ampc-set-dirty 'playlists t)
              (ampc-set-dirty 'playlist t))
             (playlist
              (ampc-set-dirty 'current-playlist t)
              (ampc-set-dirty 'status t)))
        end
        do (forward-line))
  (ampc-update))

(defun ampc-handle-setup (status)
  (unless (and (string-match "^ MPD \\(.+\\)\\.\\(.+\\)\\.\\(.+\\)$"
                             status)
               (let ((version-a (string-to-number (match-string 1 status)))
                     (version-b (string-to-number (match-string 2 status)))
                     ;; (version-c (string-to-number (match-string 2 status)))
                     )
                 (or (> version-a 0)
                     (>= version-b 15))))
    (error (concat "Your version of MPD is not supported.  "
                   "ampc supports MPD protocol version 0.15.0 "
                   "and later"))))

(defun ampc-fill-internal-db (running)
  (loop with tree = (assoc (ampc-tags) ampc-internal-db)
        with tags =
        (loop for w in (ampc-normalize-windows)
              for props = (with-current-buffer (window-buffer w)
                            (when (eq (car ampc-type) 'tag)
                              (ampc-set-dirty t)
                              (plist-get (cdr ampc-type) :tag)))
              when props
              collect props
              end)
        with song-props = (ampc-with-buffer 'song
                            (ampc-set-dirty t)
                            (plist-get (cdr ampc-type) :properties))
        for origin = (and (search-forward-regexp "^file: " nil t)
                          (line-beginning-position))
        then next
        while origin
        do (goto-char (1+ origin))
        for next = (and (search-forward-regexp "^file: " nil t)
                        (line-beginning-position))
        while (or (not running) next)
        do (save-restriction
             (narrow-to-region origin (or next (point-max)))
             (ampc-fill-internal-db-entry tree tags song-props))
        (when running
          (delete-region origin next)
          (setf next origin))))

(defun ampc-tags ()
  (loop for w in (ampc-normalize-windows)
        for tag = (with-current-buffer (window-buffer w)
                    (when (eq (car ampc-type) 'tag)
                      (plist-get (cdr ampc-type) :tag)))
        when tag
        collect tag
        end))

(defun ampc-fill-internal-db-entry (tree tags song-props)
  (loop for tag in tags
        for data = (ampc-clean-tag tag (ampc-extract tag))
        do (unless (cdr tree)
             (setf (cdr tree) (ampc-create-tree)))
        (setf tree (avl-tree-enter (cdr tree)
                                   (cons data nil)
                                   (lambda (_ match)
                                     match))))
  (push (cons (cons "file" (ampc-extract "file"))
              (loop for p in song-props
                    for data = (ampc-clean-tag (car p) (ampc-extract (car p)))
                    when data
                    collect (cons (car p) data)
                    end))
        (cdr tree)))

(defun ampc-handle-current-song ()
  (loop for k in (append ampc-status-tags '("Artist" "Title" "file"))
        for s = (ampc-extract k)
        do (when s
             (push (cons (intern k) s) ampc-status)))
  (ampc-fill-status)
  (run-hook-with-args ampc-status-changed-hook ampc-status))

(defun ampc-handle-status ()
  (loop for k in '("volume" "repeat" "random" "consume" "xfade" "state" "song")
        for v = (ampc-extract k)
        do (when v
             (push (cons (intern k) v) ampc-status)))
  (ampc-with-buffer 'current-playlist
    (when ampc-highlight-current-song-mode
      (font-lock-fontify-region (point-min) (point-max)))))

(defun ampc-handle-update ()
  (message "Database update started"))

(defun ampc-handle-command (status)
  (cond
   ((eq status 'error)
    (pop ampc-outstanding-commands))
   ((eq status 'running)
    (case (caar ampc-outstanding-commands)
      (listallinfo (ampc-fill-internal-db t))))
   (t
    (case (car (pop ampc-outstanding-commands))
      (idle
       (ampc-handle-idle))
      (setup
       (ampc-handle-setup status))
      (currentsong
       (ampc-handle-current-song))
      (status
       (ampc-handle-status))
      (update
       (ampc-handle-update))
      (listplaylistinfo
       (ampc-fill-playlist))
      (listplaylists
       (ampc-fill-playlists))
      (playlistinfo
       (ampc-fill-current-playlist))
      (mini-playlistinfo
       (ampc-mini-impl))
      (mini-currentsong
       (ampc-status))
      (listallinfo
       (ampc-handle-listallinfo))
      (outputs
       (ampc-fill-outputs)))
    (unless ampc-outstanding-commands
      (ampc-update)))))

(defun ampc-handle-listallinfo ()
  (ampc-fill-internal-db nil)
  (ampc-set-dirty 'tag t)
  (ampc-set-dirty 'song t))

(defun ampc-filter (_process string)
  (assert (buffer-live-p (process-buffer ampc-connection)))
  (with-current-buffer (process-buffer ampc-connection)
    (when string
      (when (and ampc-debug (not (eq ampc-debug t)))
        (message "ampc: <- %s" string))
      (goto-char (process-mark ampc-connection))
      (insert string)
      (set-marker (process-mark ampc-connection) (point)))
    (save-excursion
      (goto-char (point-min))
      (let ((success))
        (if (or (progn
                  (when (search-forward-regexp
                         "^ACK \\[\\(.*\\)\\] {.*} \\(.*\\)\n\\'"
                         nil
                         t)
                    (message "ampc command error: %s (%s; %s)"
                             (match-string 2)
                             (match-string 1)
                             (funcall (if ampc-debug 'identity 'car)
                                      (car ampc-outstanding-commands)))
                    t))
                (when (search-forward-regexp "^OK\\(.*\\)\n\\'" nil t)
                  (setf success t)))
            (progn
              (let ((match-end (match-end 0)))
                (save-restriction
                  (narrow-to-region (point-min) match-end)
                  (goto-char (point-min))
                  (ampc-handle-command (if success (match-string 1) 'error)))
                (delete-region (point-min) match-end))
              (unless ampc-no-implicit-next-dispatch
                (ampc-send-next-command))))
        (ampc-handle-command 'running)))))

(defun* ampc-set-tab-offsets
    (&rest properties &aux (min 2) (optional-padding 0))
  (unless properties
    (return-from ampc-set-tab-offsets))
  (set (make-local-variable 'tab-stop-list) nil)
  (loop for (title . props) in properties
        for min- = (plist-get props :min)
        do (incf min (or (plist-get props :width) min-))
        (when min-
          (incf optional-padding (- (plist-get props :max) min-))))
  (loop for (title . props) in properties
        with offset = 2
        do (push offset tab-stop-list)
        (incf offset (or (plist-get props :width)
                         (let ((min- (plist-get props :min))
                               (max (plist-get props :max)))
                           (if (>= min (window-width))
                               min-
                             (min max
                                  (+ min-
                                     (floor (* (/ (float (- max min-))
                                                  optional-padding)
                                               (- (window-width)
                                                  min))))))))))
  (callf nreverse tab-stop-list))

(defun* ampc-configure-frame-1 (split &aux (split-type (car split)))
  (if (memq split-type '(vertical horizontal))
      (let* ((sizes))
        (loop with length = (if (eq split-type 'horizontal)
                                (window-total-width)
                              (window-total-height))
              with rest = length
              with rest-car
              for (size . subsplit) in (cdr split)
              do (if (equal size 1.0)
                     (progn (push t sizes)
                            (setf rest-car sizes))
                   (let ((l (if (integerp size) size (round (* size length)))))
                     (decf rest l)
                     (push l sizes)))
              finally do (setf (car rest-car) rest))
        (let ((first-window (selected-window)))
          (callf nreverse sizes)
          (loop for size in (copy-sequence sizes)
                for window on (cdr sizes)
                do (select-window
                    (setf (car window)
                          (split-window nil size (eq split-type 'horizontal)))))
          (setf (car sizes) first-window))
        (loop for subsplit in (cdr split)
              for window in sizes
              with result
              do (with-selected-window window
                   (setf result
                         (or (ampc-configure-frame-1 (cdr subsplit)) result)))
              finally return result))
    (setf (window-dedicated-p (selected-window)) nil)
    (pop-to-buffer-same-window
     (get-buffer-create
      (concat "*"
              (mapconcat (lambda (s) (concat (upcase (substring s 0 1))
                                             (substring s 1)))
                         (if (memq split-type '(tag song))
                             (list (or (plist-get (cdr split) :tag) "song"))
                           (split-string (symbol-name split-type) "-"))
                         " ")
              "*")))
    (if (memq split-type '(tag song))
        (ampc-tag-song-mode)
      (let ((mode (intern (concat "ampc-" (symbol-name split-type) "-mode"))))
        (unless (fboundp mode)
          (setf mode 'ampc-mode))
        (unless (eq major-mode 'mode)
          (funcall mode))))
    (destructuring-bind
        (&key (properties nil) (dedicated t) (mode-line t) &allow-other-keys)
        (cdr split)
      (apply 'ampc-set-tab-offsets properties)
      (setf ampc-type split
            (window-dedicated-p (selected-window)) dedicated
            mode-line-format (when mode-line
                               (default-value 'mode-line-format))))
    (set (make-local-variable 'mode-line-buffer-identification)
         '(:eval (let ((result
                        (concat (car-safe (propertized-buffer-identification
                                           (buffer-name)))
                                (when ampc-dirty
                                  " [Updating...]"))))
                   (if (< (length result) 12)
                       (concat result (make-string (- 12 (length result)) ? ))
                     result))))
    (ampc-update-header)
    (add-to-list 'ampc-all-buffers (current-buffer))
    (push (cons (or (plist-get (cdr split) :id) 9999) (selected-window))
          ampc-windows)
    (ampc-set-dirty t)
    (when (plist-get (cdr split) :select)
      (selected-window))))

(defun* ampc-configure-frame
    (split &optional no-update &aux (old-selection ampc-type) old-window-starts)
  (loop for w in (ampc-normalize-windows)
        do (with-selected-window w
             (with-current-buffer (window-buffer w)
               (push (cons (current-buffer) (window-start))
                     old-window-starts))))
  (if (not ampc-use-full-frame)
      (ampc-restore-window-configuration)
    (setf (window-dedicated-p (selected-window)) nil)
    (delete-other-windows))
  (setf ampc-windows nil)
  (let ((select-window (ampc-configure-frame-1 split)))
    (setf ampc-windows
          (mapcar (lambda (window)
                    (cons window (window-buffer window)))
                  (mapcar 'cdr (sort ampc-windows
                                     (lambda (a b) (< (car a) (car b)))))))
    (loop for w in (ampc-normalize-windows)
          do (with-selected-window w
               (let ((old-window-start (cdr (assq (current-buffer)
                                                  old-window-starts))))
                 (when old-window-start
                   (set-window-start nil old-window-start)))
               (when (and (derived-mode-p 'ampc-item-mode)
                          (> (length tab-stop-list) 1))
                 (ampc-set-dirty 'erase))))
    (select-window (or (loop for w in (ampc-normalize-windows)
                             thereis
                             (when (equal (with-current-buffer (window-buffer w)
                                            ampc-type)
                                          old-selection)
                               w))
                       select-window
                       (selected-window))))
  (unless no-update
    (ampc-update)))

(defun ampc-move-to-tab ()
  "Move point to next logical tab stop."
  (interactive)
  (let ((tab (loop for tab in
                   (or (get-text-property (point) 'tab-stop-list) tab-stop-list)
                   while (>= (current-column) tab)
                   finally return tab)))
    (when tab
      (goto-char (min (+ (line-beginning-position) tab) (line-end-position))))))

(defun ampc-mouse-play-this (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-play-this))

(defun ampc-mouse-delete (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-delete 1))

(defun ampc-mouse-add (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-add-impl))

(defun ampc-mouse-delete-playlist (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-delete-playlist t))

(defun ampc-mouse-load (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-load t))

(defun ampc-mouse-toggle-output-enabled (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-toggle-output-enabled 1))

(defun* ampc-mouse-toggle-mark (event &aux (inhibit-read-only t))
  (interactive "e")
  (let ((window (posn-window (event-end event))))
    (when (with-selected-window window
            (goto-char (posn-point (event-end event)))
            (unless (eobp)
              (move-beginning-of-line nil)
              (ampc-mark-impl (not (eq (char-after) ?*)) 1)
              t))
      (select-window window))))

(defun ampc-mouse-align-point (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-align-point))

(defun* ampc-unmark-all (&aux (inhibit-read-only t))
  "Remove all marks."
  (interactive)
  (assert (ampc-in-ampc-p))
  (save-excursion
    (goto-char (point-min))
    (loop while (search-forward-regexp "^\\* " nil t)
          do (replace-match "  " nil nil)))
  (ampc-post-mark-change-update))

(defun ampc-trigger-update ()
  "Trigger a database update."
  (interactive)
  (assert (ampc-on-p))
  (ampc-send-command 'update))

(defun* ampc-toggle-marks (&aux (inhibit-read-only t))
  "Toggle marks.
Marked entries become unmarked, and vice versa."
  (interactive)
  (assert (ampc-in-ampc-p))
  (save-excursion
    (loop for (a . b) in '(("* " . "T ")
                           ("  " . "* ")
                           ("T " . "  "))
          do (goto-char (point-min))
          (loop while (search-forward-regexp (concat "^" (regexp-quote a))
                                             nil
                                             t)
                do (replace-match b nil nil))))
  (ampc-post-mark-change-update))

(defun ampc-up (&optional arg)
  "Go to the previous ARG'th entry.
With optional prefix ARG, move the next ARG entries after point
rather than the selection."
  (interactive "P")
  (assert (ampc-in-ampc-p))
  (ampc-move t arg))

(defun ampc-down (&optional arg)
  "Go to the next ARG'th entry.
With optional prefix ARG, move the next ARG entries after point
rather than the selection."
  (interactive "P")
  (assert (ampc-in-ampc-p))
  (ampc-move nil arg))

(defun ampc-mark (&optional arg)
  "Mark the next ARG'th entries.
ARG defaults to 1."
  (interactive "p")
  (assert (ampc-in-ampc-p))
  (ampc-mark-impl t arg))

(defun ampc-unmark (&optional arg)
  "Unmark the next ARG'th entries.
ARG defaults to 1."
  (interactive "p")
  (assert (ampc-in-ampc-p))
  (ampc-mark-impl nil arg))

(defun ampc-set-volume (&optional arg)
  "Set volume to ARG percent.
If ARG is nil, read ARG from minibuffer."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-volume-impl (or arg (read-number "Volume: "))))

(defun ampc-increase-volume (&optional arg)
  "Increase volume by prefix argument ARG or, if ARG is nil,
`ampc-volume-step'."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-volume-impl arg '+))

(defun ampc-decrease-volume (&optional arg)
  "Decrease volume by prefix argument ARG or, if ARG is nil,
`ampc-volume-step'."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-volume-impl arg '-))

(defun ampc-set-crossfade (&optional arg)
  "Set crossfade to ARG seconds.
If ARG is nil, read ARG from minibuffer."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-crossfade-impl (or arg (read-number "Crossfade: "))))

(defun ampc-increase-crossfade (&optional arg)
  "Increase crossfade by prefix argument ARG or, if ARG is nil,
`ampc-crossfade-step'."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-crossfade-impl arg '+))

(defun ampc-decrease-crossfade (&optional arg)
  "Decrease crossfade by prefix argument ARG or, if ARG is nil,
`ampc-crossfade-step'."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-crossfade-impl arg '-))

(defun ampc-toggle-repeat (&optional arg)
  "Toggle MPD's repeat state.
With prefix argument ARG, enable repeating if ARG is positive,
otherwise disable it."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-toggle-state 'repeat arg))

(defun ampc-toggle-consume (&optional arg)
  "Toggle MPD's consume state.
With prefix argument ARG, enable consuming if ARG is positive,
otherwise disable it.

When consume is activated, each song played is removed from the playlist."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-toggle-state 'consume arg))

(defun ampc-toggle-random (&optional arg)
  "Toggle MPD's random state.
With prefix argument ARG, enable random playing if ARG is positive,
otherwise disable it."
  (interactive "P")
  (ampc-toggle-state 'random arg))

(defun ampc-play-this (&optional arg)
  "Play selected song.
With prefix argument ARG, play the ARG'th song located at the
zero-indexed position of the current playlist."
  (interactive "P")
  (assert (and (ampc-on-p) (or arg (ampc-in-ampc-p))))
  (if (not arg)
      (unless (eobp)
        (ampc-send-command 'play nil (1- (line-number-at-pos)))
        (ampc-send-command 'pause nil 0))
    (ampc-send-command 'play nil arg)
    (ampc-send-command 'pause nil 0)))

(defun* ampc-toggle-play
    (&optional arg &aux (state (cdr (assq 'state ampc-status))))
  "Toggle play state.
If MPD does not play a song already, start playing the song at
point if the current buffer is the playlist buffer, otherwise
start at the beginning of the playlist.

If ARG is 4, stop player rather than pause if applicable."
  (interactive "P")
  (assert (ampc-on-p))
  (unless state
    (return-from ampc-toggle-play))
  (when arg
    (setf arg (prefix-numeric-value arg)))
  (ecase (intern state)
    (stop
     (when (or (null arg) (> arg 0))
       (ampc-send-command
        'play
        nil
        (if (and (eq (car ampc-type) 'current-playlist) (not (eobp)))
            (1- (line-number-at-pos))
          0))))
    (pause
     (when (or (null arg) (> arg 0))
       (ampc-send-command 'pause nil 0)))
    (play
     (cond ((or (null arg) (< arg 0))
            (ampc-send-command 'pause nil 1))
           ((eq arg 4)
            (ampc-send-command 'stop))))))

(defun ampc-next (&optional arg)
  "Play next song.
With prefix argument ARG, skip ARG songs."
  (interactive "p")
  (assert (ampc-on-p))
  (ampc-skip (or arg 1)))

(defun ampc-previous (&optional arg)
  "Play previous song.
With prefix argument ARG, skip ARG songs."
  (interactive "p")
  (assert (ampc-on-p))
  (ampc-skip (- (or arg 1))))

(defun ampc-rename-playlist (new-name)
  "Rename selected playlist to NEW-NAME.
Interactively, read NEW-NAME from the minibuffer."
  (interactive "MNew name: ")
  (assert (ampc-in-ampc-p))
  (if (ampc-playlist)
      (ampc-send-command 'rename nil (ampc-playlist) new-name)
    (error "No playlist selected")))

(defun ampc-load (&optional at-point)
  "Load selected playlist in the current playlist.
If optional argument AT-POINT is non-nil (or if no playlist is
selected), use playlist at point rather than the selected one."
  (interactive)
  (assert (ampc-in-ampc-p))
  (if (ampc-playlist at-point)
      (ampc-send-command 'load nil (ampc-quote (ampc-playlist at-point)))
    (if at-point
        (error "No playlist at point")
      (error "No playlist selected"))))

(defun ampc-toggle-output-enabled (&optional arg)
  "Toggle the next ARG outputs.
If ARG is omitted, use the selected entries."
  (interactive "P")
  (assert (ampc-in-ampc-p))
  (ampc-with-selection arg
    (let ((data (get-text-property (point) 'data)))
      (ampc-send-command (if (equal (cdr (assoc "outputenabled" data)) "1")
                             'disableoutput
                           'enableoutput)
                         nil
                         (cdr (assoc "outputid" data))))))

(defun ampc-delete (&optional arg)
  "Delete the next ARG songs from the playlist.
If ARG is omitted, use the selected entries.  If ARG is non-nil,
all marks after point are removed nontheless."
  (interactive "P")
  (assert (ampc-in-ampc-p))
  (let ((point (point)))
    (ampc-with-selection arg
      (let ((val (1- (- (line-number-at-pos) index))))
        (if (ampc-playlist)
            (ampc-send-command 'playlistdelete
                               t
                               (ampc-quote (ampc-playlist))
                               val)
          (ampc-send-command 'delete t val))))
    (goto-char point)
    (ampc-align-point)))

(defun ampc-shuffle ()
  "Shuffle playlist."
  (interactive)
  (assert (ampc-on-p))
  (if (not (ampc-playlist))
      (ampc-send-command 'shuffle)
    (ampc-with-buffer 'playlist
      (let ((shuffled
             (mapcar 'car
                     (sort (loop
                            until (eobp)
                            collect (cons (cdr (assoc "file" (get-text-property
                                                              (+ 2 (point))
                                                              'data)))
                                          (random))
                            do (forward-line))
                           (lambda (a b)
                             (< (cdr a) (cdr b)))))))
        (ampc-clear)
        (loop for s in shuffled
              do (ampc-add-impl s))))))

(defun ampc-clear ()
  "Clear playlist."
  (interactive)
  (assert (ampc-on-p))
  (if (ampc-playlist)
      (ampc-send-command 'playlistclear nil (ampc-quote (ampc-playlist)))
    (ampc-send-command 'clear)))

(defun ampc-add (&optional arg)
  "Add the songs associated with the next ARG entries after point
to the playlist.
If ARG is omitted, use the selected entries in the current buffer."
  (interactive "P")
  (assert (ampc-in-ampc-p))
  (ampc-with-selection arg
    (ampc-add-impl)))

(defun* ampc-status (&optional no-print)
  "Display and return the information that is displayed in the status window.
If optional argument NO-PRINT is non-nil, just return the text.
If NO-PRINT is nil, the display may be delayed if ampc does not
have enough information yet."
  (interactive)
  (assert (ampc-on-p))
  (unless (or ampc-status no-print)
    (ampc-send-command 'status t)
    (ampc-send-command 'mini-currentsong t)
    (return-from ampc-status))
  (let* ((flags (mapconcat
                 'identity
                 (loop for (f . n) in '((repeat . "Repeat")
                                        (random . "Random")
                                        (consume . "Consume"))
                       when (equal (cdr (assq f ampc-status)) "1")
                       collect n
                       end)
                 "|"))
         (state (cdr (assq 'state ampc-status)))
         (status (concat "State:     " state
                         (when (and ampc-yield no-print)
                           (concat (make-string (- 10 (length state)) ? )
                                   (nth (% ampc-yield 4) '("|" "/" "-" "\\"))))
                         "\n"
                         (when (equal state "play")
                           (concat "Playing:   "
                                   (or (cdr (assq 'Artist ampc-status))
                                       "[Not Specified]")
                                   " - "
                                   (or (cdr (assq 'Title ampc-status))
                                       "[Not Specified]")
                                   "\n"))
                         "Volume:    " (cdr (assq 'volume ampc-status)) "\n"
                         "Crossfade: " (cdr (assq 'xfade ampc-status))
                         (unless (equal flags "")
                           (concat "\n" flags)))))
    (unless no-print
      (message "%s" status))
    status))

(defun ampc-delete-playlist (&optional at-point)
  "Delete selected playlist.
If optional argument AT-POINT is non-nil (or if no playlist is
selected), use playlist at point rather than the selected one."
  (interactive)
  (assert (ampc-in-ampc-p))
  (if (ampc-playlist at-point)
      (when (y-or-n-p (concat "Delete playlist " (ampc-playlist at-point) "?"))
        (ampc-send-command 'rm nil (ampc-quote (ampc-playlist at-point))))
    (if at-point
        (message "No playlist at point")
      (message "No playlist selected"))))

(defun ampc-store (name)
  "Store current playlist as NAME.
Interactively, read NAME from the minibuffer."
  (interactive "MSave playlist as: ")
  (assert (ampc-in-ampc-p))
  (ampc-send-command 'save nil (ampc-quote name)))

(defun* ampc-goto-current-song (&aux (song (cdr (assq 'song ampc-status))))
  "Select the current playlist window and move point to the current song."
  (interactive)
  (assert (ampc-in-ampc-p))
  (let ((window (ampc-with-buffer 'current-playlist
                  (selected-window))))
    (when window
      (select-window window)
      (when song
        (goto-char (point-min))
        (forward-line (string-to-number song)))
      (ampc-align-point))))

(defun ampc-previous-line (&optional arg)
  "Go to previous ARG'th entry in the current buffer.
ARG defaults to 1."
  (interactive "p")
  (assert (ampc-in-ampc-p))
  (ampc-next-line (* (or arg 1) -1)))

(defun ampc-next-line (&optional arg)
  "Go to next ARG'th entry in the current buffer.
ARG defaults to 1."
  (interactive "p")
  (assert (ampc-in-ampc-p))
  (forward-line arg)
  (if (eobp)
      (progn (forward-line -1)
             (forward-char 2)
             t)
    (ampc-align-point)
    nil))

(defun* ampc-suspend (&optional (run-hook t))
  "Suspend ampc.
This function resets the window configuration, but does not close
the connection to MPD or destroy the internal cache of ampc.
This means subsequent startups of ampc will be faster."
  (interactive)
  (when ampc-working-timer
    (cancel-timer ampc-working-timer))
  (ampc-restore-window-configuration)
  (loop for b in ampc-all-buffers
        do (when (buffer-live-p b)
             (kill-buffer b)))
  (setf ampc-windows nil
        ampc-all-buffers nil
        ampc-working-timer nil)
  (when run-hook
    (run-hooks 'ampc-suspend-hook)))

(defun ampc-mini ()
  "Select song to play via `completing-read'."
  (interactive)
  (assert (ampc-on-p))
  (ampc-send-command 'mini-playlistinfo t))

(defun ampc-quit (&optional arg)
  "Quit ampc.
If prefix argument ARG is non-nil, kill the MPD instance that
ampc is connected to."
  (interactive "P")
  (when (ampc-on-p)
    (set-process-filter ampc-connection nil)
    (when (equal (car-safe ampc-outstanding-commands) '(idle))
      (ampc-send-command-impl "noidle")
      (with-current-buffer (process-buffer ampc-connection)
        (loop do (goto-char (point-min))
              until (search-forward-regexp "^\\(ACK\\)\\|\\(OK\\).*\n\\'" nil t)
              do (accept-process-output ampc-connection nil 50))))
    (ampc-send-command-impl (if arg "kill" "close")))
  (when ampc-working-timer
    (cancel-timer ampc-working-timer))
  (ampc-suspend nil)
  (setf ampc-connection nil
        ampc-internal-db nil
        ampc-outstanding-commands nil
        ampc-status nil)
  (run-hooks 'ampc-quit-hook))

;;;###autoload
(defun ampc-suspended-p ()
  "Return non-nil if ampc is suspended."
  (interactive)
  (and (ampc-on-p) (not ampc-windows)))

;;;###autoload
(defun ampc-on-p ()
  "Return non-nil if ampc is connected to the daemon."
  (interactive)
  (and ampc-connection (memq (process-status ampc-connection) '(open run))))

;;;###autoload
(defun ampc (&optional host port suspend)
  "ampc is an asynchronous client for the MPD media player.
This function is the main entry point for ampc.

HOST and PORT specify the MPD instance to connect to.  The values
default to the ones specified in `ampc-default-server'."
  (interactive)
  (unless (byte-code-function-p (symbol-function 'ampc))
    (message "You should byte-compile ampc"))
  (run-hooks 'ampc-before-startup-hook)
  (unless host
    (setf host (or (car ampc-default-server) (read-string  "Host: "))))
  (unless port
    (setf port (or (cdr ampc-default-server) (read-string "Port: "))))
  (when (and ampc-connection
             (or (not (equal host ampc-host))
                 (not (equal port ampc-port))
                 (not (ampc-on-p))))
    (ampc-quit))
  (unless ampc-connection
    (let ((connection (open-network-stream "ampc"
                                           (with-current-buffer
                                               (get-buffer-create " *ampc*")
                                             (erase-buffer)
                                             (current-buffer))
                                           host
                                           port
                                           :type 'plain :return-list t)))
      (unless (car connection)
        (error "Failed connecting to server: %s"
               (plist-get ampc-connection :error)))
      (setf ampc-connection (car connection)
            ampc-host host
            ampc-port port))
    (set-process-coding-system ampc-connection 'utf-8-unix 'utf-8-unix)
    (set-process-filter ampc-connection 'ampc-filter)
    (set-process-query-on-exit-flag ampc-connection nil)
    (setf ampc-outstanding-commands '((setup))))
  (if suspend
      (ampc-update)
    (ampc-configure-frame (cddar ampc-views)))
  (run-hooks 'ampc-connected-hook)
  (when suspend
    (ampc-suspend))
  (ampc-filter (process-buffer ampc-connection) nil))

(provide 'ampc)

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; outline-regexp: ";;; \\*+"
;; fill-column: 80
;; indent-tabs-mode: nil
;; End:
