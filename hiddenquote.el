;;; hiddenquote.el --- Major mode for doing hidden quote puzzles -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023  Free Software Foundation, Inc.

;; Author: Mauro Aranda <maurooaranda@gmail.com>
;; Maintainer: Mauro Aranda <maurooaranda@gmail.com>
;; Created: Sun Dec 13 11:10:00 2020
;; Version: 1.2
;; Package-Version: 1.2
;; Package-Requires: ((emacs "25.1"))
;; URL: https://mauroaranda.com/puzzles/hidden-quote-puzzle/
;; Keywords: games

;; This file is NOT part of GNU Emacs.

;; hiddenquote is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; hiddenquote is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with hiddenquote. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; hiddenquote is a kind of word puzzle where you have to read
;; word definitions (the clues), and put the answers into a grid, using
;; the provided syllables.  When the grid is complete, you should be able
;; to read a quote in the highlighted cells.
;;
;; This word puzzle is known in Argentina as Claringrilla, because they are
;; published by a national newspaper named "Clarín".
;;
;; This file contains all the hidden quote puzzle rendering, major mode
;; and commands.  It also defines a source to retrieve puzzles of
;; the hidden quote type, created by myself.
;;
;;;; Usage:
;; M-x hiddenquote
;; Select the source you want to retrieve the puzzle from.
;; With a prefix argument, the command will prompt for a specific ID number.
;;
;; Read the clues in the Definitions buffer, and complete each word with
;; one character per cell to complete the puzzle.  For each word, mark the
;; syllables it contains as used.
;; Move around the buffer with the usual movement commands.
;; Edit each character cell with the usual editing commands.
;; To mark a syllable as used, click it with the mouse or move point to
;; it and hit RET.
;; When the puzzle is complete, you should be able to read the hidden
;; quote in the highlighted cells.
;;
;; M-x customize-group RET hiddenquote
;; to see other customization options.
;; To see what faces you can customize, type
;; M-x customize-group RET hiddenquote-faces
;; M-x describe-keymap RET hiddenquote-mode-map
;; or
;; M-x describe-keymap RET hiddenquote-character-map
;; to see the commands available.
;;
;; To change the puzzle sources, customize `hiddenquote-sources'.

;;; Code:

;; Requirements.
(require 'eieio)
(require 'eieio-base)
(require 'cl-lib)
(require 'wid-edit)
(require 'subr-x)
(require 'json)

(defvar json-pretty-print-max-secs) ; json.el
(defvar url-mime-accept-string) ; url-vars.el

;; Customization.
(defgroup hiddenquote nil
  "Solve hiddenquote puzzles in Emacs."
  :group 'games
  :version "0.1")

(defcustom hiddenquote-sources
  '(("puzzles" ipuz hiddenquote-get-local-puzzle)
    ("hidden-quote" ipuz hiddenquote-get-hidden-quote-puzzle))
  "An alist of hidden quote puzzle sources.
Each member is of the form (SOURCE-NAME ORIG-FORMAT FUNCTION).

SOURCE-NAME should be a string, a unique name for the source/publisher.
ORIG-FORMAT should be a symbol naming the format in which the puzzle
is retrieved.  At the moment, there is builtin support for two formats:
ipuz format and html format.

FUNCTION should be a function that retrieves an instance of
`hiddenquote-hidden-quote-puzzle'.  The function should take one argument,
which is the prefix arg given to `hiddenquote'."
  :type '(repeat :tag "Hidden Quote Sources"
                 (choice
                  (list :tag "IPUZ"
                        (string :tag "Source name")
                        (const :tag "Format: ipuz" ipuz)
                        (function :tag "Custom function"))
                  (list :tag "HTML"
                        (string :tag "Source name")
                        (const :tag "Format: html" html)
                        (function :tag "Function"))
                  (list :tag "Custom source"
                        (string :tag "Source name")
                        (symbol :tag "Format")
                        (function :tag "Custom function"))))
  :package-version '(hiddenquote . "0.2"))

(defcustom hiddenquote-mode-hook nil
  "Hook to run after entering `hiddenquote-mode'."
  :type 'hook
  :package-version '(hiddenquote . "0.1"))

(defcustom hiddenquote-buffer-name-format "*Hidden Puzzle %n*"
  "Format to use for the buffer name.

Supported escapes:
%t: Insert the puzzle title.
%n: Insert the puzzle number."
  :type 'string
  :package-version '(hiddenquote . "0.1"))

(defcustom hiddenquote-upcase-chars (display-graphic-p)
  "Non-nil if you don't want chars to get upcased automatically.

In TTYs, it is recommended to set this option to nil,
so characters like \"á\" can be displayed without problem.  This is relevant
only if the puzzles you play contain such characters, of course."
  :type 'boolean
  :package-version '(hiddenquote . "0.1"))

(defcustom hiddenquote-automatic-check nil
  "Non-nil means check after each change if the answer is right or wrong."
  :type 'boolean
  :package-version '(hiddenquote . "0.1"))

(defcustom hiddenquote-directory
  (file-name-as-directory (locate-user-emacs-file "hiddenquote"))
  "Directory where to store the puzzles' files."
  :type 'directory
  :package-version '(hiddenquote . "0.1"))

(defcustom hiddenquote-show-time t
  "Whether to show the time passed since starting playing a puzzle."
  :type 'boolean
  :package-version '(hiddenquote . "0.1"))

(defcustom hiddenquote-skip-used-syllables nil
  "If non-nil, tabbing skips used syllables in the Syllables buffer."
  :type 'boolean
  :package-version '(hiddenquote . "0.1"))

(defcustom hiddenquote-skip-definitions-window t
  "If non-nil, `other-window' and the like skips the Definitions window.
This is non-nil by default, so that `other-window' takes you quicker to the
Syllables window."
  :type 'boolean
  :package-version '(hiddenquote . "1.2")
  :set (lambda (sym val)
         (set sym val)
         (when (boundp 'hiddenquote-buffer) ; defvar further down.
           (cl-loop for buff in (buffer-list)
                    when (with-current-buffer buff
                           (and hiddenquote-buffer
                                (string-match "- Definitions$"
                                              (buffer-name buff))))
                    do (set-window-parameter (get-buffer-window buff)
                                             'no-other-window val)))))

(defcustom hiddenquote-after-last-character-hook nil
  "Hook that runs after entering the last character in a word.

Note that this doesn't mean the word is the right word.

It can be handy to run `other-window' in this hook, so you can go directly
to the syllables buffer."
  :type 'hook
  :package-version '(hiddenquote . "1.3"))  

(defcustom hiddenquote-mark-syllable-after-isearch t
  "Whether to mark a syllable as used/unused after exiting Isearch with RET.

When non-nil, hiddenquote will add a function to `isearch-mode-end-hook' to
allow running the command after exiting Isearch."
  :type 'boolean
  :package-version '(hiddenquote . "1.3"))

(defgroup hiddenquote-faces nil
  "Faces used by `hiddenquote'."
  :group 'hiddenquote)

(defface hiddenquote
  '((t (:inherit default :height 1.5)))
  "Base face used by `hiddenquote'."
  :package-version '(hiddenquote . "0.1"))

(defface hiddenquote-title
  '((((class color) (background light))
     (:foreground "#444" :height 0.75 :inherit hiddenquote))
    (((class color) (background dark))
     (:foreground "white" :height 0.75 :inherit hiddenquote))
    (t (:inherit hiddenquote)))
  "Face used for the title."
  :package-version '(hiddenquote . "0.1"))

(defface hiddenquote-sep
  '((t (:height 0.1 :inherit default)))
  "Face used in character-separating spaces."
  :package-version '(hiddenquote . "0.1"))

(defface hiddenquote-word-number
  '((((type tty))
     (:foreground "black" :background "white" :inherit hiddenquote))
    (((class color))
     (:foreground "white" :background "#999" :inherit hiddenquote))
    (t (:inherit hiddenquote)))
  "Face used for word indexes."
  :package-version '(hiddenquote . "0.1"))

(defface hiddenquote-word-right
  '((t (:foreground "white" :background "#5adc5f" :inherit hiddenquote)))
  "Face used for correct answers."
  :package-version '(hiddenquote . "0.1"))

(defface hiddenquote-word-wrong
  '((t (:foreground "white" :background "#d80026" :inherit hiddenquote)))
  "Face used for wrong answers."
  :package-version '(hiddenquote . "0.1"))

(defface hiddenquote-value
  '((t (:foreground "#333" :background "gray80" :inherit hiddenquote)))
  "Face used for characters inserted by the user."
  :package-version '(hiddenquote . "0.1"))

(defface hiddenquote-quote-value
  '((t (:foreground "#333" :background "#fcee21" :inherit hiddenquote)))
  "Face used for the characters in the grid that hold the quote."
  :package-version '(hiddenquote . "0.1"))

(defface hiddenquote-used-syllable
  '((t (:foreground "black" :background "yellow" :inherit hiddenquote)))
  "Face used for syllables that the user has used."
  :package-version '(hiddenquote . "0.1"))

(defface hiddenquote-unused-syllable
  '((t (:inherit hiddenquote)))
  "Face used for syllables that the user hasn't used."
  :package-version '(hiddenquote . "0.1"))

(defface hiddenquote-highlight-syllable
  '((t (:background "#fcee21" :height 1.5 :inherit default)))
  "Face used for highlighting syllables when the mouse hovers."
  :package-version '(hiddenquote . "0.1"))

(defface hiddenquote-widget-highlight
  '((t (:background "#fcee21" :inherit hiddenquote)))
  "Face used for highlighting a widget."
  :package-version '(hiddenquote . "0.1"))

(defface hiddenquote-highlight
  '((((class color) (background light))
     (:background "#fcee21" :inherit 'default))
    (((class color) (background dark))
     (:background "#fcee21" :foreground "black"))
    (t (:inherit default)))
  "Face used for highlighting the current definition."
  :package-version '(hiddenquote . "0.1"))

(defface hiddenquote-doc
  '((t (:foreground "#999" :inherit default)))
  "Face used for the puzzle commentary."
  :package-version '(hiddenquote . "0.1"))

;; Variables.
(defvar hiddenquote-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Mouse support.
    (define-key map [down-mouse-2] #'widget-button-click)
    (define-key map [down-mouse-1] #'widget-button-click)
    ;; Widget movement.
    (define-key map "\t" #'widget-forward)
    (define-key map [right] #'hiddenquote-forward)
    (define-key map [left] #'hiddenquote-backward)
    (define-key map [(shift tab)] #'hiddenquote-backward)
    (define-key map [backtab] #'hiddenquote-backward)
    (define-key map "\C-n" #'hiddenquote-next)
    (define-key map [down] #'hiddenquote-next)
    (define-key map "\C-p" #'hiddenquote-prev)
    (define-key map [up] #'hiddenquote-prev)
    (define-key map [(meta ?g) (meta ?g)] #'hiddenquote-goto-word)
    (define-key map [(meta ?g) (control ?s)] #'hiddenquote-search-word)
    ;; Done.
    (define-key map [(control ?x) (control ?s)] #'hiddenquote-save)
    (define-key map [(control ?x) ?!] #'hiddenquote-give-up)
    (define-key map [(control ?x) ?k] #'hiddenquote-quit)
    map)
  "Keymap for the buffer where the grid is at.")

(defvar hiddenquote-character-map
  (let ((map (copy-keymap hiddenquote-mode-map)))
    ;; Movement.
    (define-key map "\C-f" #'widget-forward)
    (define-key map [right] #'widget-forward)
    (define-key map "\C-b" #'widget-backward)
    (define-key map [left] #'widget-backward)
    (define-key map "\C-a" #'hiddenquote-move-beginning-of-word)
    (define-key map [home] #'hiddenquote-move-beginning-of-word)
    (define-key map "\C-e" #'hiddenquote-move-end-of-word)
    (define-key map [end] #'hiddenquote-move-end-of-word)
    (define-key map "\C-j" #'hiddenquote-next)
    ;; Editing.
    (define-key map "\C-d" #'hiddenquote-delete-char)
    (define-key map "\C-k" #'hiddenquote-kill-word)
    (define-key map [(control shift backspace)] #'hiddenquote-kill-whole-word)
    (define-key map [backspace] #'hiddenquote-delete-backward-char)
    (define-key map [delete] #'delete-char)
    (define-key map [?\d] #'hiddenquote-delete-backward-char)
    (define-key map "\C-t" #'hiddenquote-transpose-chars)
    ;; Actions.
    (define-key map "?" #'hiddenquote-check-answer)
    (define-key map "\C-m" #'widget-field-activate)
    ;; Avoid trouble.
    (define-key map " " #'ignore)
    (define-key map "\C-o" #'ignore)
    map)
  "Keymap for the `hiddenquote-character' widget.")

(defvar hiddenquote-syllables-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "\t" #'hiddenquote-forward-syllable)
    (define-key map [(shift tab)] #'hiddenquote-backward-syllable)
    (define-key map [backtab] #'hiddenquote-backward-syllable)
    (define-key map "f" #'hiddenquote-forward-syllable)
    (define-key map "\C-f" #'hiddenquote-forward-syllable)
    (define-key map "b" #'hiddenquote-backward-syllable)
    (define-key map "\C-b" #'hiddenquote-backward-syllable)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "a" #'hiddenquote-beg-of-line-syllable)
    (define-key map "\C-a" #'hiddenquote-beg-of-line-syllable)
    (define-key map "e" #'hiddenquote-end-of-line-syllable)
    (define-key map "\C-e" #'hiddenquote-end-of-line-syllable)
    (define-key map "\M->" #'hiddenquote-goto-last-syllable)
    (define-key map "\C-x u" #'hiddenquote-undo)
    map)
  "Keymap for the Syllables buffer.")

(defvar hiddenquote-previous-window-configuration nil
  "Holds the window configuration before `hiddenquote' creates its windows.")

(defvar-local hiddenquote-current nil
  "Holds the current puzzle, a `hiddenquote-grid' widget.")

(defvar-local hiddenquote-definition-markers nil
  "A vector of cons-cells (START . END), the START and END of a definition.

Used locally in the definitions buffer, to highlight/unhighlight them.")

(defvar-local hiddenquote-syllables nil
  "Holds all syllable widgets in the syllables buffer.")

(defvar-local hiddenquote-buffer nil "The buffer where the grid is at.")

;; Utilities.
(defun hiddenquote-insert (c face)
  "Insert C (a character or a string), with face FACE, using `widget-insert'."
  (let ((s (if (characterp c) (char-to-string c) c)))
    (widget-insert (propertize s 'face face))))

(defun hiddenquote-clear-buffer ()
  "Clear the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun hiddenquote-refuse-kill-buffer ()
  "Refuse to kill a definitions or a syllables buffer."
  (error "Can't kill %s buffer.  Kill the main puzzle buffer instead"
         (buffer-name)))

(defun hiddenquote-ensure-file-exists (file)
  "Create the file named FILE if it not exists."
  (unless (file-exists-p file)
    (let ((paren-dir (file-name-directory file)))
      (when (and paren-dir (not (file-exists-p paren-dir)))
        (make-directory paren-dir t)))
    (write-region "" nil file nil 0)))

(defun hiddenquote-expand-puzzle-file-name (filename source)
  "Use `expand-file-name' on FILENAME, using SOURCE as a relative directory."
  (expand-file-name filename
                    (expand-file-name
                     (file-name-as-directory source)
                     (file-name-as-directory hiddenquote-directory))))

(defun hiddenquote-maybe-mark-syllable-after-isearch ()
  "Mark syllable when exiting isearch with success.

Only marks the syllable if `hiddenquote-mark-syllable-after-isearch' is
non-nil."
  (when (and hiddenquote-mark-syllable-after-isearch
             (equal (this-command-keys) [return]))
    (widget-button-press (point))))

(defun hiddenquote-widget-backward ()
  "Compatibility function, to avoid `widget-backward' skipping widgets."
  (unless (eobp)
    (forward-char 1)))

;; Classes.
;; A `hiddenquote-hidden-quote-puzzle' represents a hidden quote puzzle,
;; following closely its ipuz spec.
(defclass hiddenquote-hidden-quote-puzzle ()
  (;; Mandatory.
   (version :initarg :version
            :initform "http://ipuz.org/v2"
            :type string
            :documentation "The ipuz version")
   (kind :initarg :kind
         :initform ["http://mauroaranda.com/puzzles/hiddenquote#1"
                    "http://ipuz.org/crossword#1"]
         :type vector
         :documentation "The ipuz PuzzleKind field")
   ;; Optional.
   (id :initarg :id
       :initform 1
       :type natnum
       :custom integer
       :documentation "An unique id for the ipuz file, a positive number.")
   (author :initarg :author
           :initform ""
           :type string
           :custom string
           :documentation "The ipuz file author.")
   (editor :initarg :editor
           :initform ""
           :type string
           :custom string
           :documentation "The ipuz file editor.")
   (copyright :initarg :copyright
              :initform ""
              :type string
              :documentation "Copyright for the ipuz file.")
   (publisher :initarg :publisher
              :initform ""
              :type string
              :custom string
              :documentation "Publisher of the puzzle.")
   (title :initarg :title
          :initform ""
          :type string
          :documentation "Title of the puzzle.")
   (url :initarg :url
        :initform ""
        :type string
        :documentation "Permanent url for the puzzle.")
   (origin :initarg :origin
           :initform ""
           :type string
           :custom string
           :documentation "Program that created the puzzle.")
   (created-date :initarg :created-date
                 :initform (format-time-string "%m/%d/%Y" (current-time))
                 :type string
                 :custom string
                 :documentation "Date of creation, in MM/DD/YYYY format.")
   (description :initarg :description
                :initform ""
                :type string
                :custom text
                :documentation
                "Insert documentation for the user, like whose quote it is.")
   ;; Crossword fields.
   (clues :initarg :clues
          :initform []
          :type vector
          :custom (vector
                   (repeat :inline t
                           (list (string :tag "Answer") (string :tag "Clue"))))
          :documentation
          "Clues and answers used in the puzzle.  Have to be ordered.")
   (saved :initarg :saved
          :initform nil
          :type (or null hiddenquote-edit)
          :documentation "Saved state for the puzzle.")
   ;; Hiddenquote extensions.
   (field-lengths :initarg :field-lengths
                  :initform []
                  :documentation "Hold the length of each answer."
                  :type vector)
   (arrows :initarg :arrows
           :initform ""
           :type string
           :custom string
           :documentation "Positions of highlighted characters.
It has to be of the form ARR1,ARR2")
   (syllables :initarg :syllables
              :initform []
              :type vector
              :custom (vector (repeat :inline t
                                      (string :tag "Syllable")))
              :documentation "Syllables (not necessarily sorted.)")
   (qquote :initarg :quote
           :initform ""
           :type string
           :custom string
           :documentation "The quote that is hidden in the puzzle.")
   (subject :initarg :subject
            :initform ""
            :type string
            :custom string
            :documentation
            "The subject for the puzzle (e.g., general, chemistry, etc.)")
   ;; Not part of ipuz.
   (file :initarg :file
         :initform ""
         :type string
         :documentation "Filename to save this puzzle, in ipuz format.")))

;; A `hiddenquote-edit' object is an object that holds the user edits
;; in a playing session.  This information can be saved in an .ipuz file,
;; effectively saving user progress.
(defclass hiddenquote-edit ()
  ((answers :initarg :answers
            :initform nil
            :documentation "User answers."
            :type (or vector null))
   (checked-answers :initarg :checked-answers
                    :initform nil
                    :documentation "Answers that the user has checked."
                    :type list)
   (used-syllables :initarg :used-syllables
                   :initform nil
                   :documentation "A list with the indexes of used-syllables."
                   :type list)
   (elapsed-time :initarg :elapsed-time
                 :initform 0
                 :documentation "Time elapsed while completing the puzzle."
                 :type (or number list))))

(cl-defmethod hiddenquote-create-grid ((self hiddenquote-hidden-quote-puzzle))
  "Create a grid to complete SELF.

Returns the `hiddenquote-grid' widget created."
  (let* ((w (widget-create 'hiddenquote-grid self)) ; Create the grid.
         (def-buff (get-buffer-create (concat (buffer-name) " - Definitions")))
         (cbuff (current-buffer))
         (inhibit-read-only t)
         window)
    ;; Fill the doc, so it looks better.
    (fill-region (overlay-start (widget-get w :doc-overlay))
                 (overlay-end (widget-get w :doc-overlay)))
    ;; Put point at the first word.
    (goto-char (point-min))
    ;; Create definitions.
    (with-current-buffer def-buff
      (erase-buffer)
      (add-hook 'kill-buffer-hook #'hiddenquote-refuse-kill-buffer nil t)
      (hiddenquote-insert "Definitions\n\n" 'hiddenquote-title)
      (let* ((defs (mapcar (lambda (el)
                             (cadr el))
                           (oref self clues)))
             (i 0)
             markers opoint)
        (dolist (def defs)
          (setq opoint (point))
          ;; Used to be "%d. %s." (notice the dot), but that looked weird
          ;; when definitions already had a full stop.
          (insert (format "%d. %s" (setq i (1+ i)) def))
          ;; So insert "." conditionally.
          (unless (char-equal (char-before) ?.)
            (insert "."))
          (fill-region opoint (point))
          (push (cons (copy-marker opoint) (point-marker)) markers)
          (insert "\n\n"))
        (setq hiddenquote-definition-markers (vconcat (reverse markers))))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (setq hiddenquote-buffer cbuff))
    (setq window (split-window nil nil 'right))
    (set-window-buffer window def-buff)
    (set-window-dedicated-p window t)
    (when hiddenquote-skip-definitions-window
      (set-window-parameter window 'no-other-window t))
    ;; Syllables.
    (let ((syll-buff (get-buffer-create (concat (buffer-name) " - Syllables")))
          (syllables (oref self syllables))
          (used (and (oref self saved)
                     (oref (oref self saved) used-syllables))))
      (with-current-buffer syll-buff
        (kill-all-local-variables)
        (hiddenquote-clear-buffer)
        (remove-overlays)
        (let ((i 0)
              (max (apply #'max (mapcar (lambda (el)
                                          (length el))
                                        syllables))))
          (setq hiddenquote-syllables
                (cl-mapcar
                 (lambda (syl)
                   ;; FIXME: Maybe it looks better center-aligned.
                   (let ((w (widget-create
                             'hiddenquote-syllable
                             :tag (concat syl (make-string
                                               (1+ (- max (length syl)))
                                               ?\s))
                             :pressed-face 'hiddenquote-highlight-syllable
                             :value (member i used))))
                     (when (= 0 (% (setq i (1+ i)) 7))
                       (insert "\n"))
                     w))
                 syllables)))
        ;; Avoid highlighting the last syllable too much.
        (hiddenquote-insert " " 'hiddenquote-sep)
        (setq-local widget-button-click-moves-point t)
        (use-local-map hiddenquote-syllables-map)
        (widget-setup)
        (setq buffer-read-only t)
        (setq hiddenquote-buffer cbuff)
        (add-hook 'kill-buffer-hook #'hiddenquote-refuse-kill-buffer nil t)
        (add-hook 'isearch-mode-end-hook
                  #'hiddenquote-maybe-mark-syllable-after-isearch nil t)
        (when (< emacs-major-version 28)
          (add-hook 'widget-backward-hook
                    #'hiddenquote-widget-backward nil t))
        (goto-char (point-min)))
      (set-window-buffer (split-window window) syll-buff))
    ;; In case we loaded an already completed puzzle.
    (unless (hiddenquote-puzzle-complete-p)
      (hiddenquote-timer-start-timer (widget-get w :hiddenquote-timer)))
    w))

(cl-defmethod hiddenquote-puzzle-to-ipuz
  ((self hiddenquote-hidden-quote-puzzle))
  "Return a string that is an ipuz representation of SELF."
  (let ((width (1+ (apply #'max (cl-mapcar (lambda (clue)
                                             (length (nth 0 clue)))
                                           (oref self clues)))))
        (height (length (oref self clues)))
        (arrows (let ((lst (split-string (oref self arrows) ",")))
                  (cons (string-to-number (car lst))
                        (string-to-number (cadr lst)))))
        (edits (oref self saved)))
    (with-temp-buffer
      (insert
       (json-encode `(("version" . ,(oref self version))
                      ("kind" . ,(oref self kind))
                      ("copyright" . ,(oref self copyright))
                      ("publisher" . ,(oref self publisher))
                      ("url" . ,(oref self url))
                      ("uniqueid" . ,(number-to-string (oref self id)))
                      ("title" . ,(oref self title))
                      ("author" . ,(oref self author))
                      ("editor" . ,(oref self editor))
                      ("date" . ,(oref self created-date))
                      ("origin" . ,(oref self origin))
                      ("intro" . ,(oref self description))
                      ("dimensions" . ,(list (cons "width" width)
                                             (cons "height" height)))
                      ("puzzle" .
                       ,(let ((i 1))
                          (vconcat
                           (cl-mapcar
                            (lambda (clue)
                              (prog1
                                  (let ((len (length (nth 0 clue))))
                                    (vconcat (list (number-to-string i))
                                             (make-list len 0)
                                             (when (> (- width len) 0)
                                               (make-list (- width len) nil))))
                                (setq i (1+ i))))
                            (oref self clues)))))
                      ("clues" .
                       ,(list
                         (cons "Clues"
                               (let ((i 1))
                                 (vconcat
                                  (cl-mapcar
                                   (lambda (clue)
                                     (prog1
                                         `(("number" . ,i)
                                           ("clue" . ,(nth 1 clue))
                                           ("answer" . ,(nth 0 clue))
                                           ("enumeration" . ,(length
                                                              (nth 0 clue))))
                                       (setq i (1+ i))))
                                   (oref self clues)))))))
                      ("answer" . ,(oref self qquote))
                      ("zones" .
                       ,(let ((arr1 (car arrows))
                              (arr2 (cdr arrows))
                              (fake-height (1- height)))
                          (vconcat
                           (list
                            (list
                             (cons "rect" (vector 0 0 0 fake-height))
                             (cons "style" '(("border" . 0))))
                            (list
                             (cons "rect" (vector arr1 0 arr1 fake-height))
                             (cons "style" '(("highlight" . t))))
                            (list
                             (cons "rect" (vector arr2 0 arr2 fake-height))
                             (cons "style" '(("highlight" . t))))))))
                      ("saved" .
                       ,(list (cons "answers" (if edits (oref edits answers)
                                                []))
                              (cons "checked-answers"
                                    (when edits
                                      (oref edits checked-answers)))
                              (cons "used-syllables"
                                    (when edits
                                      (oref edits used-syllables)))
                              (cons "elapsed-time"
                                    (if edits
                                        (oref edits elapsed-time)
                                      0))))
                      ("volatile" . (("com.hiddenquote:arrows" . "")
                                     ("com.hiddenquote:subject" . "")
                                     ("com.hiddenquote:syllables" . "")))
                      ("com.hiddenquote:arrows" . ,(oref self arrows))
                      ("com.hiddenquote:subject" . ,(oref self subject))
                      ("com.hiddenquote:syllables" .
                       ,(vconcat (sort (copy-sequence
                                        (oref self syllables))
                                       #'string-collate-lessp))))))
      (let ((json-pretty-print-max-secs 0.0))
        (json-pretty-print (point-min) (point-max)))
      (buffer-string))))

(cl-defmethod hiddenquote-load-saved-puzzle
  ((self hiddenquote-hidden-quote-puzzle))
  "Return a `hiddenquote-edit' instance, read from the file slot in SELF.

If the file does not exist, the `hiddenquote-edit' instance has default
values."
  (if (file-exists-p (oref self file))
      (let ((temp (hiddenquote-puzzle-from-ipuz
                   (with-current-buffer (find-file-noselect (oref self file) t)
                     (prog1 (buffer-string)
                       (kill-buffer))))))
        (oref temp saved))
    (make-instance 'hiddenquote-edit)))

(cl-defmethod hiddenquote-format-buffer-name
  ((self hiddenquote-hidden-quote-puzzle))
  "Return a buffer name, using `hiddenquote-buffer-name-format' and SELF."
  (let ((fn (if (fboundp 'string-replace)
                #'string-replace
              #'replace-regexp-in-string))
        ret)
    (setq ret (funcall fn "%t" (oref self title)
                       hiddenquote-buffer-name-format))
    (setq ret (funcall fn "%n" (if (stringp (oref self id))
                                   (oref self id)
                                 (number-to-string (oref self id)))
                       ret))))

;; Hiddenquote Widget.
;; Use a widget, `hiddenquote-grid', to hold everything.  Its tag is
;; the puzzle's title.  The value holds all the answers in the grid,
;; and the documentation is the puzzle intro.
(define-widget 'hiddenquote-grid 'default
  "A widget for a hidden quote puzzle."
  :format "%t\n%d\n%v\n%T"
  :format-handler #'hiddenquote-grid-format-handler
  :convert-widget #'hiddenquote-grid-convert-widget
  :value-create #'hiddenquote-grid-value-create)

(define-widget 'hiddenquote-word 'default
  "A widget for each answer in a `hiddenquote-grid' widget."
  :format "%v\n"
  :value-create #'hiddenquote-word-value-create
  :value-get #'hiddenquote-word-value-get
  :value-set #'hiddenquote-word-value-set
  :validate #'hiddenquote-word-validate
  :notify #'hiddenquote-word-notify)

(define-widget 'hiddenquote-number 'item
  "A clickable item.  When actioned, shows if the answer is right or wrong."
  :format "%[%t%]"
  :value 'unchecked
  :button-face-get #'hiddenquote-number-button-face-get
  :mouse-face 'hiddenquote-widget-highlight
  :pressed-face 'hiddenquote-widget-highlight
  :action #'hiddenquote-number-action)

(define-widget 'hiddenquote-character 'character
  "A character widget for the `hiddenquote-word' widget."
  :format "%v"
  :value ?\s
  :notify #'hiddenquote-character-notify
  :value-to-internal (lambda (_widget value)
                       (if (stringp value) value (char-to-string value)))
  :keymap hiddenquote-character-map
  :help-echo nil)

(define-widget 'hiddenquote-timer 'item
  "A widget that shows the elapsed time starting from its creation."
  :format "%v\n"
  :value-create #'hiddenquote-timer-value-create
  :hiddenquote-start-value 0
  :sample-face 'hiddenquote-title)

;; Use clickable-item widgets for the syllables, so the user can toggle their
;; state.  When the user marks a syllable as used, draw it with the
;; `hiddenquote-used-syllable' face, otherwise if the user marks a syllable as
;; unused, draw it with `hiddenquote-unused-syllable' face.
(define-widget 'hiddenquote-syllable 'item
  "Used/unused state for a syllable."
  :format "%[%v%]"
  :value-create #'hiddenquote-syllable-value-create
  :button-face-get #'hiddenquote-syllable-button-face-get
  :mouse-face 'hiddenquote-highlight-syllable
  :action #'hiddenquote-syllable-action
  :notify #'hiddenquote-syllable-notify)

(defun hiddenquote-grid-convert-widget (widget)
  "Initialize :tag and :doc properties of `hiddenquote-grid' widget WIDGET.

Use the number slot of a `hiddenquote-hidden-quote-puzzle' object to build
the :tag, and use the description slot to build the :doc."
  (let ((puzzle (nth 0 (widget-get widget :args))))
    (widget-put widget :hiddenquote puzzle)
    (widget-put widget :args nil) ; Don't need them anymore.
    (widget-put widget :tag (propertize
                             (oref puzzle title)
                             'face 'hiddenquote-title))
    (widget-put widget :doc (oref puzzle description)))
  widget)

(defun hiddenquote-grid-value-create (widget)
  "Create a `hiddenquote-grid' widget WIDGET, by creating each word."
  (let* ((puzzle (widget-get widget :hiddenquote))
         (lengths (oref puzzle field-lengths))
         children buttons)
    (dotimes (i (length lengths))
      ;; FIXME: Maybe it looks better center-aligned.
      (let ((opoint (point)))
        (push (widget-create-child-and-convert
               widget 'hiddenquote-number
               :hiddenquote-word-number i
               :tag (format "%-2d" (1+ i))
               :value (if (and (oref puzzle saved)
                               (oref (oref puzzle saved) checked-answers))
                          ;; `json-encode' outputs the symbols as strings,
                          ;; so get them back as symbols.
                          (intern-soft
                           (nth i (oref (oref puzzle saved) checked-answers)))
                        'unchecked))
              buttons)
        (hiddenquote-insert " " 'hiddenquote-sep)
        (add-text-properties opoint (point)
                             `(cursor-sensor-functions
                               ,(list
                                 (lambda (_window _prev action)
                                   (if (eq action 'left)
                                       (hiddenquote-unhighlight-definition i)
                                     (hiddenquote-highlight-definition i)
                                     (widget-forward 1)))))))
      (push
       (widget-create-child-and-convert
        widget 'hiddenquote-word
        :hiddenquote-word-number i
        :hiddenquote-word-length (aref lengths i)
        :value (if (and (oref puzzle saved)
                        (oref (oref puzzle saved) answers)
                        (> (length (oref (oref puzzle saved) answers)) 0))
                   (aref (oref (oref puzzle saved) answers) i)
                 (make-string (aref lengths i) ?\s)))
       children)
      (hiddenquote-insert "\n" 'hiddenquote-sep))
    (widget-put widget :buttons (reverse buttons))
    (widget-put widget :children (reverse children))))

(defun hiddenquote-grid-format-handler (widget ch)
  "Recognize escape character CH when creating a `hiddenquote-grid' WIDGET.

It handles a custom escape character, \"%T\", to create a hiddenquote-timer
widget."
  (let ((saved (oref (widget-get widget :hiddenquote) saved)))
    (cond ((eq ch ?T) ; Print time.
           (when hiddenquote-show-time
             (widget-put widget :hiddenquote-timer
                         (widget-create
                          'hiddenquote-timer
                          :hiddenquote-buffer (current-buffer)
                          :hiddenquote-start-value
                          (if (and saved (oref saved elapsed-time))
                              (if (fboundp 'time-convert)
                                  (time-convert ; Emacs 27.1
                                   (oref saved elapsed-time)
                                   'integer)
                                (oref saved elapsed-time))
                            0)))))
          (t (widget-default-format-handler widget ch)))))

(defun hiddenquote-word-value-create (widget)
  "Create a `hiddenquote-word' widget WIDGET, by creating each character cell.

Highlight the character cells that form the quote."
  (let ((arrows (split-string
                 (oref (widget-get (widget-get widget :parent) :hiddenquote)
                       arrows)
                 ","))
        (opoint (point))
        (value (widget-get widget :value))
        children)
    (dotimes (i (widget-get widget :hiddenquote-word-length))
      (push
       (widget-create-child-and-convert
        widget 'hiddenquote-character
        :value (aref value i)
        :value-face (if (or (= (1+ i) (string-to-number (car arrows)))
                            (= (1+ i) (string-to-number (cadr arrows))))
                        'hiddenquote-quote-value
                      'hiddenquote-value))
       children)
      (unless (eql i (1- (widget-get widget :hiddenquote-word-length)))
        (hiddenquote-insert " " 'hiddenquote-sep)))
    (add-text-properties opoint (point)
                         `(cursor-sensor-functions
                           ,(list
                             (lambda (_window _prev action)
                               (if (eq action 'entered)
                                   (hiddenquote-highlight-definition
                                    (widget-get widget
                                                :hiddenquote-word-number))
                                 (hiddenquote-unhighlight-definition
                                  (widget-get widget
                                              :hiddenquote-word-number)))))))
    (widget-put widget :children (reverse children))))

(defun hiddenquote-word-value-get (widget)
  "Return `hiddenquote-word' widget WIDGET's value."
  (apply #'string (mapcar #'widget-value (widget-get widget :children))))

(defun hiddenquote-word-value-set (widget value)
  "Set the value of `hiddenquote-word' widget WIDGET to VALUE."
  (let ((children (widget-get widget :children)))
    (if (nthcdr (1- (length value)) children)
        (let ((i 0))
          (dolist (child children)
            (widget-value-set child (aref value i))
            (setq i (1+ i))))
      (error "Expected string of %d characters" (length children)))))

(defun hiddenquote-after-change (from to _old)
  "Compatibility `after-change-function' for Emacs < 28.

Starting from Emacs 28.1, `widget-after-change' passes a fake event to the
field :notify function.  We use that fake event to figure out what to do.

Notify the widget between FROM and TO about a change."
  (let ((field (widget-field-find from))
        (other (widget-field-find to)))
    (when field
      (unless (eq field other)
        (error "Change in different fields"))
      (widget-apply field :notify field (list 'after-change from to)))))

(defun hiddenquote-word-notify (widget child event)
  "Notify the `hiddenquote-word' widget WIDGET about a change in CHILD.

If the car of EVENT is `before-change', this function just calls
`widget-default-notify'.  If the car of EVENT is `after-change',
advances point to some other widget and maybe check the answer."
  ;; When CHILD is the last WIDGET's children, go to the first child.
  (when (and (eq (car-safe event) 'after-change)
             (not (eql (nth 1 event) (nth 2 event))))
    (if (eq child (car (last (widget-get widget :children))))
        (progn
          (goto-char (overlay-start
                      (widget-get (car (widget-get widget :children))
                                  :field-overlay)))
          (run-hooks 'hiddenquote-after-last-character-hook)) 
      (widget-forward 1)))
  (when (and (eq (car-safe event) 'after-change)
             (or hiddenquote-automatic-check
                 (not
                  (eq (widget-value
                       (nth (widget-get widget :hiddenquote-word-number)
                            (widget-get (widget-get widget :parent) :buttons)))
                      'unchecked))))
    (hiddenquote-check-answer))
  (widget-default-notify widget child event))

(defun hiddenquote-word-validate (widget)
  "Nil if the current value of the hiddenquote-word WIDGET is correct."
  (let ((val (widget-value widget))
        (puzzle (widget-get (widget-get widget :parent) :hiddenquote)))
    (unless (string-collate-equalp
             val
             (car (aref (oref puzzle clues)
                        (widget-get widget :hiddenquote-word-number)))
             nil t)
      widget)))

(defun hiddenquote-character-notify (widget child &optional event)
  "Replace characters in character widget WIDGET, so its size is always 1.

Checks that the car of EVENT is `after-change'.

Calls `widget-default-notify' with WIDGET, CHILD and EVENT as args."
  (let ((start (nth 1 event))
        (end (nth 2 event))
        (parent (widget-get widget :parent))
        val)
    (when (eq (car-safe event) 'after-change)
      (if (eql start end)
          ;; Hack! Restore the cursor-sensor-functions property here,
          ;; because `widget-value-set' drops it.
          (add-text-properties (1- (widget-get parent :from))
                               (widget-get parent :to)
                               `(cursor-sensor-functions
                                 ,(list
                                   (lambda (_window _prev action)
                                     (if (eq action 'entered)
                                         (hiddenquote-highlight-definition
                                          (widget-get
                                           parent
                                           :hiddenquote-word-number))
                                       (hiddenquote-unhighlight-definition
                                        (widget-get
                                         parent
                                         :hiddenquote-word-number)))))))
        (setq val (buffer-substring-no-properties start (1+ start)))
        (unless (string= val " ")
          (widget-value-set widget (if hiddenquote-upcase-chars
                                       (upcase val)
                                     val)))
        ;; Hack! Restore the cursor-sensor-functions property here,
        ;; because `widget-value-set' drops it.
        (add-text-properties (1- (widget-get parent :from))
                             (widget-get parent :to)
                             `(cursor-sensor-functions
                               ,(list
                                 (lambda (_window _prev action)
                                   (if (eq action 'entered)
                                       (hiddenquote-highlight-definition
                                        (widget-get
                                         parent
                                         :hiddenquote-word-number))
                                     (hiddenquote-unhighlight-definition
                                      (widget-get
                                       parent
                                       :hiddenquote-word-number)))))))))
    (widget-default-notify widget child event)))

(defun hiddenquote-number-action (widget &optional _event)
  "Check the answer for the word number that widget WIDGET belongs to."
  (let ((word (nth (widget-get widget :hiddenquote-word-number)
                   (widget-get (widget-get widget :parent) :children)))
        (inhibit-modification-hooks t))
    (if (string-match " " (widget-value word))
        (widget-value-set widget 'unchecked)
      (widget-value-set widget (if (widget-apply word :validate)
                                   'wrong
                                 'right)))
    ;; Hack! Restore the cursor-sensor-functions property here,
    ;; because `widget-value-set' drops it.
    (add-text-properties (widget-get widget :from)
                         (widget-get widget :to)
                         `(cursor-sensor-functions
                           ,(list
                             (lambda (_window _prev action)
                               (if (eq action 'left)
                                   (hiddenquote-unhighlight-definition
                                    (widget-get widget
                                                :hiddenquote-word-number))
                                 (hiddenquote-highlight-definition
                                  (widget-get widget
                                              :hiddenquote-word-number))
                                 (widget-forward 1))))))))

(defun hiddenquote-number-button-face-get (widget)
  "Return the face to use by the `hiddenquote-number' widget WIDGET."
  (let ((val (widget-value widget)))
    (cond ((eq val 'unchecked) 'hiddenquote-word-number)
          ((eq val 'right) 'hiddenquote-word-right)
          ((eq val 'wrong) 'hiddenquote-word-wrong)
          ;; Shouldn't happen.
          (t 'hiddenquote-word-number))))

(defun hiddenquote-timer-update (widget)
  "Update `hiddenquote-timer' widget WIDGET."
  (condition-case nil
      (with-current-buffer (widget-get widget :hiddenquote-buffer)
        (save-excursion
          (goto-char (widget-get widget :from))
          (let ((elapsed (time-since (widget-get widget :hiddenquote-start))))
            (setq elapsed (if (fboundp 'time-convert)
                              (time-convert elapsed 'integer)
                            (+ (* 65536 (nth 0 elapsed))
                               (nth 1 elapsed)
                               (* (nth 2 elapsed) 1e-6)
                               (* (nth 3 elapsed) 1e-12))))
            (widget-value-set widget elapsed)
            (oset (oref (widget-get hiddenquote-current :hiddenquote) saved)
                  elapsed-time elapsed))))
    (error (hiddenquote-timer-stop-timer))))

(defun hiddenquote-timer-value-create (widget)
  "Create the `hiddenquote-timer' widget WIDGET."
  (let* ((startedp (widget-get widget :hiddenquote-start))
         (elapsed (if startedp
                      (widget-get widget :value)
                    (if (fboundp 'time-convert) ; Emacs 27.1
                        (time-convert
                         (widget-get widget :hiddenquote-start-value)
                         'integer)
                      (widget-get widget :hiddenquote-start-value)))))
    (with-current-buffer (widget-get widget :hiddenquote-buffer)
      (hiddenquote-insert (format-seconds "%h:%m:%s"
                                           (if (numberp elapsed)
                                               ;; Probably in Emacs 27.1
                                               elapsed
                                             ;; Probably in Emacs < 27, so
                                             ;; compute it by hand.
                                             (+ (* 65536 (nth 0 elapsed))
                                                (nth 1 elapsed)
                                                (* (nth 2 elapsed) 1e-6)
                                                (* (nth 3 elapsed) 1e-12))))
                           (widget-apply widget :sample-face-get)))))

(defun hiddenquote-syllable-value-create (widget)
  "Create the `hiddenquote-syllable' widget WIDGET."
  (let ((val (widget-value widget))
        (text (widget-get widget :tag)))
    (hiddenquote-insert text (if val
                                  'hiddenquote-used-syllable
                                'hiddenquote-unused-syllable))))

(defun hiddenquote-syllable-button-face-get (widget)
  "Return the face to use by the `hiddenquote-syllable' widget WIDGET.

Return `hiddenquote-used-syllable' if WIDGET's value is non-nil,
`hiddenquote-unused-syllable' otherwise."
  (if (widget-value widget)
      'hiddenquote-used-syllable
    'hiddenquote-unused-syllable))

(defun hiddenquote-syllable-notify (_widget _child &optional _event)
  "Check if all syllables are marked as used."
  (when (hiddenquote-puzzle-complete-p)
    (hiddenquote-timer-stop-timer)
    (message "Congratulations, you won!")))

(defun hiddenquote-syllable-undo (widget value)
  "Undo function, that sets WIDGET to VALUE and records an entry to redo it.

Redo it means setting WIDGET to the opposite of VALUE."
  (let ((undo-entry
         (list 'apply #'hiddenquote-syllable-undo widget (not value))))
    (widget-value-set widget value)
    (push undo-entry buffer-undo-list)))

(defun hiddenquote-syllable-action (widget &optional event)
  "Action the hiddenquote-syllable WIDGET upon EVENT, recording an undo entry."
  (let ((undo-entry
         (list 'apply #'hiddenquote-syllable-undo
               widget (widget-value widget))))
    (widget-toggle-action widget event)
    (push undo-entry buffer-undo-list)))

;; Functions.
(defun hiddenquote-undo ()
  "Simple undo for hiddenquote, binding `buffer-read-only' to non-nil."
  (interactive)
  (let ((buffer-read-only nil))
    (undo)))

(defun hiddenquote--get-quote-length ()
  "Return the quote length, by looking at each word in the widget."
  ;; We have to do this when the qquote slot is empty.
  (let* ((puzzle (widget-get hiddenquote-current :hiddenquote))
         (words (widget-get hiddenquote-current :children))
         (arrows (split-string (oref puzzle arrows) ","))
         (len (* (length arrows) (length words)))
         (first-word (car words))
         (last-word (car (last words))))
    (unless (> (widget-get last-word :hiddenquote-word-length)
               (string-to-number (car (last arrows))))
      (setq len (1- len)))
    (unless (> (widget-get first-word :hiddenquote-word-length)
               (string-to-number (car arrows)))
      (setq len (1- len)))
    len))

(defun hiddenquote-puzzle-from-ipuz (ipuz)
  "Return a `hiddenquote-hidden-quote-puzzle' instance specified by IPUZ."
  (let* ((json (json-read-from-string ipuz))
         (num (alist-get 'uniqueid json))
         (author (alist-get 'author json))
         (editor (alist-get 'editor json))
         (publisher (alist-get 'publisher json))
         (copyright (alist-get 'copyright json))
         (title (alist-get 'title json))
         (created-date (alist-get 'date json))
         (desc (alist-get 'intro json))
         (qquote (alist-get 'answer json))
         (clues (alist-get 'Clues (alist-get 'clues json)))
         (words (vconcat
                 (cl-mapcar
                  (lambda (clue)
                    (list (alist-get 'answer clue) (alist-get 'clue clue)))
                  clues)))
         (arrows (alist-get 'com.hiddenquote:arrows json))
         (lengths (vconcat (cl-mapcar (lambda (clue)
                                        (alist-get 'enumeration clue))
                                      clues)))
         (syllables (alist-get 'com.hiddenquote:syllables json))
         (saved (let ((spec (alist-get 'saved json)))
                  (make-instance
                   'hiddenquote-edit
                   :answers (alist-get 'answers spec)
                   :checked-answers (append (alist-get 'checked-answers spec)
                                            nil)
                   :used-syllables (append (alist-get 'used-syllables spec)
                                           nil)
                   :elapsed-time (or (alist-get 'elapsed-time spec) 0)))))
    (make-instance 'hiddenquote-hidden-quote-puzzle
                   :id (string-to-number num)
                   :author (or author "")
                   :editor (or editor "")
                   :copyright (or copyright "")
                   :publisher (or publisher "")
                   :quote qquote
                   :description desc
                   :title title
                   :clues words
                   :arrows arrows
                   :created-date created-date
                   :field-lengths lengths
                   :syllables syllables
                   :saved saved)))

(defun hiddenquote-get-local-puzzle (&optional n)
  "Return a puzzle from this package `puzzles' directory.

With N non-nil, prompt for a puzzle N°, otherwise return the newest one."
  (let* ((num (and n (read-number "Enter a puzzle number: ")))
         (dir (file-name-directory (locate-library "hiddenquote")))
         (file (if num
                   (expand-file-name (format "%s.ipuz" num)
                                     (file-name-as-directory
                                      (expand-file-name "puzzles" dir)))
                 (let* ((files
                         (directory-files
                          (expand-file-name "puzzles"
                                            (file-name-directory
                                             (locate-library "hiddenquote")))
                          t "[0-9]+.ipuz$" t))
                        (nums
                         (sort (mapcar
                                (lambda (filename)
			          (string-match "\\([0-9]+\\).ipuz$" filename)
			          (string-to-number
                                   (match-string 1 filename)))
                                files)
                               #'>)))
                   (expand-file-name (format "%s.ipuz" (car nums))
                                     (file-name-as-directory
                                      (expand-file-name "puzzles" dir))))))
         (saved-file (hiddenquote-expand-puzzle-file-name
                      (file-name-nondirectory file) "hidden-quote"))
         puzzle)
    (setq puzzle (hiddenquote-puzzle-from-ipuz
                  (with-current-buffer
                      (find-file-noselect (if (file-exists-p saved-file)
                                              saved-file
                                            file)
                                          t)
                    (prog1 (buffer-string)
                      (kill-buffer)))))
    puzzle))

(defun hiddenquote-get-hidden-quote-puzzle (&optional n)
  "Return a puzzle from the hidden-quote puzzle source.

With N nil, return the latest puzzle.  With N non-nil, prompt for a
puzzle Nº."
  (let* ((num (and n (read-number "Enter a puzzle number: ")))
         (url "https://mauroaranda.com/puzzles/hidden-quote-puzzle/")
         (file (and num (hiddenquote-expand-puzzle-file-name
                         (format "%s.ipuz" num) "hidden-quote")))
         (url-mime-accept-string "application/json")
         puzzle)
    (if (and file (file-exists-p file)) ; Look for the file locally first.
        (progn
          (setq puzzle
                (hiddenquote-puzzle-from-ipuz
                 (with-current-buffer (find-file-noselect file t)
                   (prog1 (buffer-string)
                     (kill-buffer)))))
          (oset puzzle file file)
          puzzle)
      (and num (setq url (concat url (number-to-string num))))
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (re-search-forward "\n\n")
        (setq puzzle (hiddenquote-puzzle-from-ipuz
                      (buffer-substring (point) (point-max))))
        (oset puzzle file (hiddenquote-expand-puzzle-file-name
                           (format "%s.ipuz" (oref puzzle id)) "hidden-quote"))
        (oset puzzle saved (hiddenquote-load-saved-puzzle puzzle))
        puzzle))))

(defun hiddenquote-highlight-definition (n)
  "Highlight the Nth definition."
  (with-current-buffer (get-buffer (concat (buffer-name) " - Definitions"))
    (let ((inhibit-read-only t))
      (set-text-properties (car (aref hiddenquote-definition-markers n))
                           (cdr (aref hiddenquote-definition-markers n))
                           '(face hiddenquote-highlight))
      (when-let* ((w (get-buffer-window (current-buffer))))
        (set-window-point w (car (aref hiddenquote-definition-markers n)))
        (or (pos-visible-in-window-p
             (cdr (aref hiddenquote-definition-markers n)) w)
            (with-selected-window w
              (recenter)))))))

(defun hiddenquote-unhighlight-definition (n)
  "Unhighlight the Nth definition."
  (with-current-buffer (get-buffer (concat (buffer-name) " - Definitions"))
    (let ((inhibit-read-only t))
      (set-text-properties (car (aref hiddenquote-definition-markers n))
                           (cdr (aref hiddenquote-definition-markers n))
                           '(face default)))))

(defun hiddenquote-timer-start-timer (widget)
  "Start the `hiddenquote-timer' widget WIDGET."
  (unless (widget-get widget :hiddenquote-start) ; Alreay started.
    (let ((elapsed (if (fboundp 'time-convert) ; Emacs 27.1
                       (time-convert
                        (widget-get widget :hiddenquote-start-value)
                        'integer)
                     (widget-get widget :hiddenquote-start-value)))
          (timer (run-with-timer 1 1 (lambda ()
                                       (hiddenquote-timer-update widget)))))
      (widget-put widget :hiddenquote-start
                  (time-subtract (current-time) elapsed))
      (widget-put widget :hiddenquote-timer timer)
      (add-hook 'kill-buffer-hook #'hiddenquote-timer-stop-timer nil t))))

(defun hiddenquote-timer-stop-timer ()
  "Stop the hiddenquote timer."
  (when hiddenquote-buffer
    (with-current-buffer hiddenquote-buffer
      (let ((timer (widget-get
                    (widget-get hiddenquote-current :hiddenquote-timer)
                    :hiddenquote-timer)))
        (when (timerp timer)
          (cancel-timer timer))))))

(defun hiddenquote-puzzle-complete-p ()
  "Non-nil if the grid is complete."
  (when-let* ((used-all-p (with-current-buffer
                             (concat (buffer-name hiddenquote-buffer)
                                     " - Syllables")
                           (cl-every #'widget-value hiddenquote-syllables))))
    (with-current-buffer hiddenquote-buffer
      (cl-notany (lambda (w) (widget-apply w :validate))
                 (widget-get hiddenquote-current :children)))))

(defun hiddenquote-initialize ()
  "Initialize variables and modes needed by `hiddenquote'."
  (erase-buffer)
  (remove-overlays)
  (setq hiddenquote-previous-window-configuration
        (current-window-configuration))
  (delete-other-windows)
  (cursor-sensor-mode 1)
  (setq widget-documentation-face 'hiddenquote-doc)
  (setq truncate-lines t)
  ;; Compatibility.
  (when (< emacs-major-version 28)
    (add-hook 'after-change-functions #'hiddenquote-after-change nil t)
    (add-hook 'widget-backward-hook #'hiddenquote-widget-backward nil t)))

;; Hiddenquote mode.
(easy-menu-define hiddenquote-menu (list hiddenquote-mode-map
                                          hiddenquote-character-map)
  "Menu for hiddenquote."
  '("Hiddenquote"
    ["Automatic checking" hiddenquote-toggle-automatic-check
     :style radio :selected hiddenquote-automatic-check]
    ["Manual checking" hiddenquote-toggle-automatic-check
     :style radio :selected (not hiddenquote-automatic-check)]
    ["Give up" hiddenquote-give-up]
    ["Save progress" hiddenquote-save]
    ["Quit" hiddenquote-quit]))

(easy-menu-define hiddenquote-character-menu hiddenquote-character-map
  "Menu for hiddenquote, when inside a character cell."
  '("Hiddenquote"
    ["Check answer" hiddenquote-check-answer]))

(defvar hiddenquote-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item-from-menu #'hiddenquote-give-up "cancel" map
                                   hiddenquote-mode-map)
    (tool-bar-local-item-from-menu #'hiddenquote-save "save" map
                                   hiddenquote-mode-map)
    (tool-bar-local-item-from-menu #'hiddenquote-quit "exit" map
                                   hiddenquote-mode-map)
    (define-key-after map [sep] menu-bar-separator)
    (tool-bar-local-item-from-menu #'hiddenquote-check-answer "search" map
                                   hiddenquote-character-map)
    map)
  "Tool-bar support for hiddenquote.")

(define-derived-mode hiddenquote-mode nil "Hiddenquote"
  "Major mode for `hiddenquote'.

Buffer bindings:
\\{hiddenquote-mode-map}

Character cell bindings:
\\{hiddenquote-character-map}."
  (when (boundp 'tool-bar-map)
    (setq-local tool-bar-map hiddenquote-tool-bar-map)))

;; Commands.
(defun hiddenquote-delete-char ()
  "Replacement for `delete-char' inside `hiddenquote-character' widgets.

Doesn't really delete the character, rather it changes it for an empty space."
  (interactive)
  (let ((w (widget-at)))
    (widget-value-set w " ")))

(defun hiddenquote-kill-word ()
  "Starting at point, delete the rest of the characters of the current word."
  (interactive)
  (let* ((child (widget-at))
         (parent (widget-get (widget-at) :parent)))
    (save-excursion
      (dolist (w (member child (widget-get parent :children)))
        (widget-value-set w " ")))))

(defun hiddenquote-kill-whole-word ()
  "Delete each character in a word."
  (interactive)
  (let ((parent (widget-get (widget-at) :parent)))
    (save-excursion
      (dolist (child (widget-get parent :children))
        (widget-value-set child " ")))))

(defun hiddenquote-delete-backward-char ()
  "Delete the previous character."
  (interactive)
  (widget-backward 1)
  (unless (eq (widget-type (widget-at)) 'hiddenquote-character)
    (widget-forward 1))
  (widget-value-set (widget-at) " ")
  (widget-backward 1))

(defun hiddenquote-transpose-chars ()
  "Interchange the characters around point, and move forward one character.

Attempting to transpose the first character in a word results in an error.

Note that after transposing the last character in a word, this command moves
point to the next word."
  (interactive)
  (let* ((ch1 (widget-at))
         (parent (widget-get ch1 :parent))
         (children (widget-get parent :children))
         (n (seq-position children ch1))
         (ch2 (if (= n 0)
                  (user-error "Can't transpose the first character in a word")
                (nth (1- n) children)))
         (oval (widget-value ch1))
         (nval (widget-value ch2)))
    ;; Spaces are problematic.
    (widget-value-set ch1 (if (char-equal nval ?\s) " " nval))
    (widget-value-set ch2 (if (char-equal oval ?\s) " " oval))
    (hiddenquote-forward)))

(defun hiddenquote-forward ()
  "Go to the next character if at a widget, use `forward-char' otherwise."
  (interactive)
  (if (widget-at)
      (progn
        (widget-forward 1)
        (while (not (eq (widget-type (widget-at)) 'hiddenquote-character))
          (widget-forward 1)))
    (forward-char)))

(defun hiddenquote-backward ()
  "Go to the previous character if at a widget, use `backward-char' otherwise."
  (interactive)
  (if (widget-at)
      (progn
        (widget-backward 1)
        (while (not (eq (widget-type (widget-at)) 'hiddenquote-character))
          (widget-backward 1)))
    (backward-char)))

(defun hiddenquote-next ()
  "Go to the next word if at a widget, to the next line otherwise."
  (interactive)
  (if (widget-at)
      (progn
        (forward-line)
        (beginning-of-line)
        (widget-forward 1)
        (while (not (eq (widget-type (widget-at)) 'hiddenquote-character))
          (widget-forward 1)))
    (forward-line)))

(defun hiddenquote-prev ()
  "Go to the previous word if at a widget, to the previous line otherwise."
  (interactive)
  (if (widget-at)
      (progn
        (forward-line -2)
        (beginning-of-line)
        (widget-forward 1)
        (while (not (eq (widget-type (widget-at)) 'hiddenquote-character))
          (widget-forward 1)))
    (forward-line -1)))

(defun hiddenquote-move-beginning-of-word ()
  "Go to the first character of the word where point is at."
  (interactive)
  (forward-line 0)
  (widget-forward 1))

(defun hiddenquote-move-end-of-word ()
  "Go to the last character of the word where point is at."
  (interactive)
  (let ((inhibit-field-text-motion t))
    (end-of-line)
    (widget-backward 1)))

(defun hiddenquote-goto-word (n)
  "Go to the Nth word."
  (interactive (list (read-number "Goto word: ")))
  (if (or (>= (1- n) (length (widget-get hiddenquote-current :buttons)))
          (< (1- n) 0))
      (user-error "Invalid word number")
    (goto-char (widget-get
                (nth (1- n) (widget-get hiddenquote-current :buttons))
                :from))))

(defun hiddenquote-forward-syllable ()
  "Move one syllable forward."
  (interactive)
  (if hiddenquote-skip-used-syllables
      (let ((orig (widget-at)))
        (widget-forward 1)
        (while (and (widget-get (widget-at) :value)
                    (not (eq orig (widget-at))))
          (widget-forward 1)))
    (widget-forward 1)))

(defun hiddenquote-backward-syllable ()
  "Move one syllable backward."
  (interactive)
  (if hiddenquote-skip-used-syllables
      (let ((orig (widget-at)))
        (widget-backward 1)
        (while (and (widget-get (widget-at) :value)
                    (not (eq orig (widget-at))))
          (widget-backward 1)))
    (widget-backward 1)))

(defun hiddenquote-beg-of-line-syllable ()
  "Move to the first syllable in the current line."
  (interactive)
  (beginning-of-line))

(defun hiddenquote-end-of-line-syllable ()
  "Move to the last syllable in the current line."
  (interactive)
  (end-of-line)
  (hiddenquote-backward-syllable))

(defun hiddenquote-goto-last-syllable ()
  "Move to the last syllable in the buffer."
  (interactive)
  (goto-char (point-max))
  (hiddenquote-backward-syllable))

(defun hiddenquote-complete-quote (str)
  "Complete the quote with the string STR.

Interactively, prompts for the string.

This command is handy when you have figured out the quote but not yet
every word."
  (interactive (list (read-string "Quote: ")))
  (setq str
        (replace-regexp-in-string "\\([[:blank:]]\\|[[:punct:]]\\)" "" str))
  ;; First, check the length.
  (let ((len (if (string-empty-p
                  (oref (widget-get hiddenquote-current :hiddenquote) qquote))
                 (hiddenquote--get-quote-length)
               (length
                (replace-regexp-in-string "\\([[:blank:]]\\|[[:punct:]]\\)" ""
                                          (oref
                                           (widget-get hiddenquote-current
                                                       :hiddenquote)
                                           qquote))))))
    (if (/= len (length str))
        (user-error "Wrong quote length")
      ;; Ok, let's set it.
      ;; Run through the string, and set each character cell value to
      ;; the right value.
      (save-excursion
        (cl-loop for ch across str
                 with i = 0
                 with words = (widget-get hiddenquote-current :children)
                 with l = (length words)
                 with puzzle = (widget-get hiddenquote-current :hiddenquote)
                 with arrows = (let ((lst (split-string
                                           (oref puzzle arrows) ",")))
                                 (cons (string-to-number (car lst))
                                       (string-to-number (cadr lst))))
                 with arrow = (car arrows)
                 with word
                 with characters
                 do
                 (when (>= i l)
                   (setq i 0
                         arrow (cdr arrows)))
                 (setq word (nth i words)
                       characters (widget-get word :children))
                 ;; This is an edge case.  It might happen that the first word
                 ;; doesn't have the 2nd arrow (or greater).  So skip it, and
                 ;; set the character in the next word.
                 ;; Of course the last word may not have the 2nd arrow
                 ;; (or greater), but by then this loop has already completed,
                 ;; so it doesn't matter.
                 (when (>= (1- arrow) (length characters))
                   (setq i (1+ i)
                         word (nth i words)
                         characters (widget-get word :children)))
                 ;; Don't mess with what's already there.
                 (when (char-equal (widget-value (nth (1- arrow) characters))
                                   ?\s)
                   (widget-value-set (nth (1- arrow) characters) ch))
                 (setq i (1+ i)))))))

(defun hiddenquote-check-answer ()
  "Check if the answer for the word where point is at is right or wrong."
  (interactive)
  (let ((parent (widget-get (widget-at) :parent)))
    (widget-apply-action (nth (widget-get parent :hiddenquote-word-number)
                              (widget-get (widget-get parent :parent)
                                          :buttons)))))

(defun hiddenquote-search-word (word)
  "Search for presence of the string WORD in a definition and go to that word."
  (interactive (list (read-string "Word: ")))
  (let* ((clues (oref (widget-get hiddenquote-current :hiddenquote) clues))
         (re (concat "\\<" (regexp-quote word) "\\>"))
         (def (seq-position clues word (lambda (clue _word)
                                         (string-match re (cadr clue))))))
    (if def
        (hiddenquote-goto-word (1+ def))
      (message "No definition contains that word"))))

(defun hiddenquote-toggle-automatic-check ()
  "Toggle the `hiddenquote-automatic-check' variable."
  (interactive)
  (customize-set-variable 'hiddenquote-automatic-check
                          (not hiddenquote-automatic-check)))

(defun hiddenquote-toggle-skip-used-syllables ()
  "Toggle the `hiddenquote-skip-used-syllables' variable."
  (interactive)
  (customize-set-variable 'hiddenquote-skip-used-syllables
                          (not hiddenquote-skip-used-syllables)))

(defun hiddenquote-save ()
  "Save the puzzle and the user progress to a file.

The file slot of the hiddenquote object contains the filename."
  (interactive)
  (let* ((puzzle (widget-get hiddenquote-current :hiddenquote))
         (user-answers (vconcat
                        (mapcar #'widget-value
                                (widget-get hiddenquote-current :children))))
         (checked-answers (mapcar #'widget-value
                                  (widget-get hiddenquote-current :buttons)))
         (used-syllables
          (with-current-buffer (concat (buffer-name) " - Syllables")
            (cl-loop for widget in hiddenquote-syllables
                     for i = 0 then (1+ i)
                     when (widget-value widget)
                     collect i)))
         (elapsed-time
          (when (widget-get hiddenquote-current :hiddenquote-timer)
            (widget-value
             (widget-get hiddenquote-current :hiddenquote-timer))))
         (edits (make-instance 'hiddenquote-edit
                               :answers user-answers
                               :checked-answers checked-answers
                               :used-syllables used-syllables
                               :elapsed-time elapsed-time)))
    (oset puzzle saved edits)
    (with-temp-buffer
      (let ((backup-inhibited t))
        (insert (hiddenquote-puzzle-to-ipuz puzzle))
        (hiddenquote-ensure-file-exists (oref puzzle file))
        (write-file (oref puzzle file))))
    (message "Saved puzzle in %s" (oref puzzle file))))

(defun hiddenquote-give-up (&optional noprompt)
  "Give up completing the puzzle, showing the solutions.

Prompts the user and shows the solutions if the answer is \"yes\".
With a prefix argument NOPROMPT non-nil, doesn't prompt."
  (interactive "P")
  (when (or noprompt (yes-or-no-p "Really give up? "))
    (save-excursion
      (with-current-buffer (concat
                            (buffer-name hiddenquote-buffer) " - Syllables")
        (dolist (syllable hiddenquote-syllables)
          (widget-value-set syllable t)))
      (let ((i 0)
            (grid hiddenquote-current))
        (dolist (child (widget-get grid :children))
          (widget-value-set
           child (car (aref (oref (widget-get grid :hiddenquote) clues) i)))
          (setq i (1+ i))))
      (hiddenquote-timer-stop-timer))))

(defun hiddenquote-quit (&optional noprompt)
  "Stop playing hiddenquote, without saving the progress.

Prompts the user and kills the buffer if the answer is \"yes\".
With a prefix argument NOPROMPT non-nil, or if the puzzle is complete,
kills the buffer unconditionally "
  (interactive "P")
  (when (or noprompt (hiddenquote-puzzle-complete-p)
            (yes-or-no-p "Really quit playing Hiddenquote? "))
    (hiddenquote-timer-stop-timer)
    (let ((name (buffer-name)))
      (dolist (buff (mapcar (lambda (suffix)
                              (concat name suffix))
                            '(" - Definitions" " - Syllables")))
        (when (get-buffer buff)
          (with-current-buffer buff
            (setq-local kill-buffer-hook nil)
            (kill-buffer))))
      (kill-buffer))
    (set-window-configuration hiddenquote-previous-window-configuration)))

;;;###autoload
(defun hiddenquote (&optional arg)
  "Start playing a Hidden Quote puzzle.

If there's only one element in `hiddenquote-sources', choose that source.
Else, prompt the user with the options in `hiddenquote-sources',
to get the puzzle to play.
ARG is passed along to the function that will get the puzzle.  To find out
how ARG is handled, read the documentation for the publisher function in
`hiddenquote-sources'.

Returns the buffer with the puzzle."
  (interactive "P")
  (let* ((source-fn (if (nthcdr 1 hiddenquote-sources)
                        (widget-choose "Choose a source"
                                       (mapcar (lambda (source)
                                                 (cons (nth 0 source)
                                                       (nth 2 source)))
                                               hiddenquote-sources))
                      (nth 2 (car hiddenquote-sources))))
         (puzzle (funcall source-fn arg))
         (buff (get-buffer-create (hiddenquote-format-buffer-name puzzle))))
    (switch-to-buffer buff)
    (hiddenquote-mode)
    (hiddenquote-initialize)
    ;; Local variables after initializing, otherwise they get killed.
    (setq hiddenquote-buffer buff)
    (setq hiddenquote-current (hiddenquote-create-grid puzzle))
    ;; Wrap up.
    (widget-setup)
    (current-buffer)))

(provide 'hiddenquote)
;;; hiddenquote.el ends here
