;;; gnosis.el --- Knowledge System  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis

;; Version: 0.10.3

;; Package-Requires: ((emacs "29.1") (compat "29.1.4.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Gnosis is a personal knowledge management and review system that
;; integrates a note-taking system with spaced repetition and
;; self-testing.  It provides Zettelkasten-style linked notes (nodes)
;; alongside spaced repetition themata, all in a single SQLite database.
;;
;; The intended workflow is:
;;
;; 1. Write notes on a topic using `gnosis-nodes-find'.
;; 2. Create themata (flashcard-like questions) related to the topic
;;    using `gnosis-add-thema'.
;; 3. Link themata to note topics by inserting node links in
;;    the keimenon (question text) or parathema (extra context) using
;;    `gnosis-nodes-insert'.
;; 4. Review themata with spaced repetition via `gnosis-review', or
;;    review all themata linked to a specific topic via
;;    `gnosis-review-topic'.
;;
;; Everything lives in one database: themata, review history,
;; nodes, node links, and thema-to-node links.
;;
;; The spaced repetition algorithm is highly adjustable, allowing
;; users to set specific values for tags, creating a personalized
;; learning environment for each topic.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'vc-git)
(require 'gnosis-sqlite)
(require 'transient)
(require 'animate)

(require 'org)
(require 'org-element)

(require 'gnosis-algorithm)
(require 'gnosis-monkeytype)
(require 'gnosis-utils)
(require 'gnosis-org)
(require 'gnosis-nodes)
(require 'gnosis-journal)


(defgroup gnosis nil
  "Spaced Repetition System For Thema Taking & Self Testing."
  :group 'external
  :prefix "gnosis-")

(defcustom gnosis-dir (locate-user-emacs-file "gnosis")
  "Gnosis directory."
  :type 'directory)

;; Directory creation deferred to gnosis--ensure-db

(defcustom gnosis-string-difference 1
  "Threshold value for string comparison in Gnosis.

This variable determines the maximum acceptable Levenshtein distance
between two strings to consider them as similar."
  :type 'integer)

(defcustom gnosis-vc-auto-push nil
  "Run `vc-push' at the end of every review session."
  :type 'boolean)

(defcustom gnosis-completing-read-function
  (cond ((or (bound-and-true-p ivy-mode)
	     (bound-and-true-p helm-mode)
	     (bound-and-true-p vertico-mode)
	     (bound-and-true-p fido-mode))
	 #'completing-read)
	(t #'ido-completing-read))
  "Function to use for `completing-read'."
  :type 'function)

(defcustom gnosis-new-themata-limit nil
  "Total new themata limit."
  :type '(choice (const :tag "None" nil)
		 (integer :tag "Number")))

(defcustom gnosis-review-new-first t
  "Review new themata first.

When nil, review new themata last."
  :type 'boolean)

(defcustom gnosis-default-average-review-period 90
  "Number of days of which the average review score will be calculated."
  :type 'integer)

(defcustom gnosis-center-content-during-review t
  "Default value for centering content during review sessions.

This is the global default used when creating new review buffers.
When non-nil, center content during review sessions.
When nil, content will be displayed left-aligned instead of centered."
  :type 'boolean)

(defcustom gnosis-latex-preview nil
  "When non-nil, render LaTeX fragments during review."
  :type 'boolean
  :group 'gnosis)

(defcustom gnosis-script-input-method-alist
  '((greek . "greek")
    (cyrillic . "cyrillic-translit"))
  "Alist mapping script symbols to input method names.
Each element is (SCRIPT . INPUT-METHOD) where SCRIPT is a symbol
from `char-script-table' and INPUT-METHOD is a string suitable for
`activate-input-method'."
  :type '(alist :key-type symbol :value-type string)
  :group 'gnosis)

(defvar-local gnosis-center-content t
  "Buffer-local variable controlling content centering.

When non-nil, center content in the current buffer.
This is set automatically based on buffer type:
- Review buffers: uses `gnosis-center-content-during-review'
- Dashboard buffers: always t (centered)
- Other buffers: defaults to t")

;;; Faces

(defface gnosis-face-separator
  '((default :inherit org-hide)
    (((background light)) :strike-through "gray70")
    (t :strike-through "gray30"))
  "Face for section separator.")

(defface gnosis-face-directions
  '((t :inherit underline))
  "Face for gnosis directions.")

(defface gnosis-face-correct
  '((t :inherit match))
  "Face for user choice.")

(defface gnosis-face-cloze
  '((t :inherit (highlight italic)))
  "Face for clozes.")

(defface gnosis-face-false
  '((t :inherit error))
  "Face for user choice.")

(defface gnosis-face-unanswered
  '((t :inherit (italic underline)))
  "Face for unanswered clozes.")

(defface gnosis-face-hint
  '((t :inherit warning))
  "Face for user choice.")

(defface gnosis-face-cloze-unanswered
  '((t :inherit underline))
  "Face for user choice.")

(defface gnosis-face-next-review
  '((t :inherit bold))
  "Face for next review.")

(defvar gnosis-db nil
  "Gnosis database connection.
Initialized lazily by `gnosis--ensure-db' on first use.")

(defun gnosis--ensure-db ()
  "Return the gnosis database connection, opening it if necessary.
Creates `gnosis-dir' and runs schema initialization on first use."
  (unless gnosis-db
    (unless (file-directory-p gnosis-dir)
      (make-directory gnosis-dir))
    (setq gnosis-db (gnosis-sqlite-open (expand-file-name "gnosis.db" gnosis-dir)))
    (gnosis-db-init))
  gnosis-db)

(autoload 'gnosis-dashboard "gnosis-dashboard" nil t)

(defvar gnosis-cloze-string "(...)")

(defvar gnosis-testing nil
  "Change this to non-nil when running manual tests.")


(defconst gnosis-db-version 8
  "Gnosis database version.")

(defvar gnosis-thema-types
  '(("Basic" . gnosis-add-thema--basic)
    ("MCQ" .  gnosis-add-thema--mcq)
    ("Double" .  gnosis-add-thema--double)
    ("Cloze" . gnosis-add-thema--cloze)
    ("MC-cloze" . gnosis-add-thema--mc-cloze))
  "Mapping of Themata & their respective functions.")

(defvar gnosis-previous-thema-tags '()
  "Tags input from previously added thema.")

(defvar gnosis-previous-thema-hint nil
  "Hint input from previously added thema.")

(defvar gnosis-due-themata-total nil
  "Total due themata.")


;; Review autoloads
(autoload 'gnosis-review "gnosis-review" nil t)
(autoload 'gnosis-review-topic "gnosis-review" nil t)
(autoload 'gnosis-review-get-due-themata "gnosis-review")
(autoload 'gnosis-review-get--due-themata "gnosis-review")
(autoload 'gnosis-review-is-due-p "gnosis-review")
(autoload 'gnosis-review-is-due-today-p "gnosis-review")
(autoload 'gnosis-review-is-thema-new-p "gnosis-review")
(autoload 'gnosis-review-get-overdue-themata "gnosis-review")
(autoload 'gnosis-review-count-overdue "gnosis-review")
(autoload 'gnosis-review-algorithm "gnosis-review")
(autoload 'gnosis-display-next-review "gnosis-review")
(autoload 'gnosis-get-linked-nodes "gnosis-review")
(autoload 'gnosis-monkeytype-start "gnosis-review" nil t)
(autoload 'gnosis-history-clear "gnosis-review" nil t)

;; Anki import autoload
(autoload 'gnosis-import-anki "gnosis-anki" nil t)

;; Export/import autoloads
(autoload 'gnosis-export--insert-thema "gnosis-export-import")
(autoload 'gnosis-export--insert-themata "gnosis-export-import")
(autoload 'gnosis-export-parse-themata "gnosis-export-import")
(autoload 'gnosis-export-db "gnosis-export-import" nil t)
(autoload 'gnosis-import-db "gnosis-export-import" nil t)
(autoload 'gnosis-save-thema "gnosis-export-import")
(autoload 'gnosis-save "gnosis-export-import" nil t)

(defvar gnosis-export-separator "\n- ")

(defvar gnosis-save-hook nil
  "Hook run after a successful `gnosis-save'.
Each function is called with the saved thema ID (integer).")

(defcustom gnosis-custom-values
  '((:tag "demo" (:proto (1 2) :anagnosis 3 :epignosis 0.5 :agnoia 0.3
			 :amnesia 0.45 :lethe 3)))
  "Custom review values for adjusting gnosis algorithm.

Each entry is a list of (:tag NAME PARAMETERS) where:
- NAME is the tag name string
- PARAMETERS is a plist with keys:
  :proto (list of integers), :anagnosis (integer),
  :epignosis (number), :agnoia (number),
  :amnesia (number 0-1), :lethe (positive integer)"
  :type '(repeat sexp)
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (gnosis-validate-custom-values value)
         (set-default symbol value))
  :group 'gnosis)

(defvar gnosis-custom--valid-values
  '(:proto :anagnosis :epignosis :agnoia :amnesia :lethe))

(defvar gnosis-review-editing-p nil
  "Boolean value to check if user is currently in a review edit.")

(defvar gnosis--id-cache nil
  "Hash table of existing thema IDs, bound during batch import.
When non-nil, `gnosis-generate-id' and `gnosis-update-thema' use this
for O(1) lookups instead of querying the database per thema.")

(defun gnosis-select (value table &optional restrictions flatten)
  "Select VALUE from TABLE, optionally with RESTRICTIONS.

Optional argument FLATTEN, when non-nil, flattens the result."
  (let* ((db (gnosis--ensure-db))
	 (cols (gnosis-sqlite--compile-columns value))
	 (where (gnosis-sqlite--compile-expr (or restrictions '(= 1 1))))
	 (sql (format "SELECT %s FROM %s WHERE %s"
		      cols (gnosis-sqlite--ident table) (car where)))
	 (output (gnosis-sqlite--select-compiled db sql (cdr where))))
    (if flatten (apply #'append output) output)))

(defun gnosis-table-exists-p (table)
  "Check if TABLE exists."
  (let* ((db (gnosis--ensure-db))
	 (tables (mapcar #'car
			 (sqlite-select db
					"SELECT name FROM sqlite_master WHERE type = 'table'"))))
    (member (gnosis-sqlite--ident table) tables)))

(defun gnosis--create-table (table &optional values)
  "Create TABLE for VALUES."
  (unless (gnosis-table-exists-p table)
    (let ((sql (format "CREATE TABLE %s (%s)"
		       (gnosis-sqlite--ident table)
		       (gnosis-sqlite--compile-schema values))))
      (gnosis-sqlite-execute (gnosis--ensure-db) sql))))

(defun gnosis--drop-table (table)
  "Drop TABLE from `gnosis-db'."
  (gnosis-sqlite-execute (gnosis--ensure-db)
			 (format "DROP TABLE %s" (gnosis-sqlite--ident table))))

(defun gnosis-drop-table (table)
  "Drop TABLE from `gnosis-db'."
  (when (gnosis-table-exists-p table)
    (gnosis--drop-table table)))

(defun gnosis--insert-into (table values &optional or-ignore)
  "Insert VALUES to TABLE.
When OR-IGNORE is non-nil, use INSERT OR IGNORE to silently skip
rows that violate a UNIQUE constraint."
  (let* ((compiled (gnosis-sqlite--compile-values values))
	 (sql (format "INSERT%s INTO %s VALUES %s"
		      (if or-ignore " OR IGNORE" "")
		      (gnosis-sqlite--ident table) (car compiled))))
    (gnosis-sqlite--execute-compiled (gnosis--ensure-db) sql (cdr compiled))))

(defun gnosis-update (table value where)
  "Update records in TABLE with to new VALUE based on the given WHERE condition.

Example:
 (gnosis-update ='themata ='(= keimenon \"NEW VALUE\") ='(= id 12))"
  (let* ((set-clause (gnosis-sqlite--compile-expr value))
	 (where-clause (gnosis-sqlite--compile-expr where))
	 (sql (format "UPDATE %s SET %s WHERE %s"
		      (gnosis-sqlite--ident table)
		      (car set-clause)
		      (car where-clause))))
    (gnosis-sqlite--execute-compiled (gnosis--ensure-db) sql
			   (append (cdr set-clause) (cdr where-clause)))))

(defun gnosis-get (value table &optional restrictions)
  "Return caar of VALUE from TABLE, optionally with where RESTRICTIONS."
  (caar (gnosis-select value table restrictions)))

(defun gnosis--delete (table &optional where)
  "Delete from TABLE, optionally restricted by WHERE clause."
  (if where
      (let ((compiled (gnosis-sqlite--compile-expr where)))
	(gnosis-sqlite--execute-compiled (gnosis--ensure-db)
			       (format "DELETE FROM %s WHERE %s"
				       (gnosis-sqlite--ident table)
				       (car compiled))
			       (cdr compiled)))
    (gnosis-sqlite--execute-compiled (gnosis--ensure-db)
			   (format "DELETE FROM %s" (gnosis-sqlite--ident table)))))

(defun gnosis-delete-thema (id &optional verification)
  "Delete thema with ID.

When VERIFICATION is non-nil, skip `y-or-n-p' prompt."
  (when (or verification (y-or-n-p "Delete thema?"))
    (gnosis-delete-themata (list id))))

(defun gnosis-delete-themata (ids)
  "Delete themata with IDS, batched to stay within SQL variable limits."
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-with-transaction db
      (dolist (table '("thema_tag" "thema_links" "review"
                       "review_log" "extras" "themata"))
        (gnosis-sqlite-execute-batch db
          (format "DELETE FROM %s WHERE %s IN (%%s)"
                  table
                  (if (string= table "thema_tag") "thema_id"
                    (if (string= table "thema_links") "source" "id")))
          ids)))))


(defun gnosis-calculate-average-daily-reviews (&optional days)
  "Calculate average reviews over the last DAYS days."
  (let* ((days (or days gnosis-default-average-review-period))
	 (dates (cl-loop for d from 0 below days
			 collect (gnosis--date-to-int (gnosis-algorithm-date (- d)))))
	 (review-counts (gnosis-select 'reviewed-total 'activity-log
				       `(and (> reviewed-total 0)
					     (in date ,(vconcat dates)))
				       t)))
    (if review-counts
	(/ (apply #'+ review-counts) (float (length review-counts)))
      0)))

(defun gnosis-shuffle (seq)
  "Shuffle SEQ."
  (cl-loop with len = (length seq)
           for i from len downto 2
           do (let ((j (random i)))  ; Get random index < i.
                (cl-rotatef (nth (1- i) seq) (nth j seq)))  ; Swap elements.
           finally return seq))

(defun gnosis-completing-read (prompt seq &optional require-match)
  "Call `gnosis-completing-read-function' with shuffled SEQ.

PROMPT: Prompt for `gnosis-completing-read-function'.
REQUIRE-MATCH: When non-nil, user must select from SEQ.
History is disabled."
  (let ((history-add-new-input nil)
	(collection (gnosis-shuffle (copy-sequence seq))))
    (if require-match
	(completing-read prompt collection nil t)
      (funcall gnosis-completing-read-function prompt collection))))

(defun gnosis-insert-separator ()
  "Insert a line separator."
  (insert "\n" (propertize " " 'display '(space :width text) 'face 'gnosis-face-separator)))

(defun gnosis-center-current-line ()
  "Centers text in the current line ignoring leading spaces."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (text (string-trim (buffer-substring start end)))
         (padding (max (/ (- (window-width) (length text)) 2) 0)))
    (delete-region start end)
    (insert (make-string padding ? ) text)))

(defun gnosis-center-string (str)
  "Center each line of STR in current window width.
Replaces links `[[source][description]]' with `description'."
  (let* ((width (window-width))
         (lines (split-string str "\n")))
    (mapconcat
     (lambda (line)
       (if (string-blank-p line)
           ""  ;; Preserve blank lines
         (let* ((trimmed (string-trim line))
                ;; Replace links with just the description part
                (processed (replace-regexp-in-string
			    "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]"
			    "\\2"
			    trimmed))
                ;; Fill the text to wrap it properly
                (wrapped (with-temp-buffer
                           (insert processed)
                           (fill-region (point-min) (point-max))
                           (buffer-string)))
                ;; Process each wrapped line with proper centering
                (wrapped-lines (split-string wrapped "\n")))
           (mapconcat
	    (lambda (wline)
	      (let ((padding (max 0 (/ (- width (string-width wline)) 2))))
                (concat (make-string padding ?\s) wline)))
	    wrapped-lines
	    "\n"))))
     lines
     "\n")))

(defun gnosis-format-string (str)
  "Format STR for display, optionally centering based on buffer preference.

When `gnosis-center-content' is non-nil, centers the text.
Otherwise, just processes org-links without centering."
  (if gnosis-center-content
      (gnosis-center-string str)
    (replace-regexp-in-string
     "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]"
     "\\2"
     str)))

(defun gnosis-apply-center-buffer-overlay (&optional point)
  "Center text in buffer starting at POINT using `gnosis-center-current-line'.
This will not be applied to sentences that start with double space.

Respects `gnosis-center-content' buffer-local setting."
  (when gnosis-center-content
    (save-excursion
      (goto-char (or point (point-min)))
      (while (not (or (= (point-max) (point)) (looking-at "^  ")))
        (gnosis-center-current-line)
        (forward-line 1)))))

(defun gnosis-org-format-string (str)
  "Return STR fontified as in `org-mode'.
When `gnosis-latex-preview' is non-nil, render LaTeX fragments as
images using `org-format-latex'."
  (with-temp-buffer
    (org-mode)
    (org-toggle-pretty-entities)
    (insert str)
    (font-lock-ensure)
    (when gnosis-latex-preview
      (condition-case err
          (progn
            (goto-char (point-min))
            (org-format-latex "ltximg/org-ltximg"
                              (point-min) (point-max) temporary-file-directory
                              'overlays nil 'forbuffer
                              org-preview-latex-default-process)
            ;; Convert overlays to text properties so they survive buffer-string
            (dolist (ov (overlays-in (point-min) (point-max)))
              (when-let ((display (overlay-get ov 'display)))
                (put-text-property (overlay-start ov) (overlay-end ov)
                                   'display display)
                (delete-overlay ov))))
        (error (message "LaTeX preview: %s" (error-message-string err)))))
    (buffer-string)))

(defun gnosis-cloze-create (str clozes &optional cloze-string)
  "Replace CLOZES in STR with CLOZE-STRING, preserving whitespace pattern."
  (cl-assert (listp clozes) nil "Adding clozes: Clozes need to be a list.")
  (let ((cloze-string (or cloze-string gnosis-cloze-string)))
    (with-temp-buffer
      (insert (gnosis-org-format-string str))
      (dolist (cloze clozes)
        (let* ((cloze-text (gnosis-utils-trim-quotes cloze))
               (replacement (concat
                             (and (string-match "^\\s-+" cloze-text)
				  (match-string 0 cloze-text))
                             (propertize cloze-string 'face 'gnosis-face-cloze)
                             (and (string-match "\\s-+$" cloze-text)
				  (match-string 0 cloze-text)))))
          (goto-char (point-min))
          (when (search-forward cloze-text nil t)
            (replace-match replacement t t))))
      (buffer-string))))

(defun gnosis-cloze-add-hints (str hints &optional cloze-string)
  "Replace CLOZE-STRING in STR with HINTS, skipping empty hints."
  (cl-assert (listp hints) nil "Hints must be a list.")
  (let ((cloze-string (or cloze-string gnosis-cloze-string)))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (cl-loop for hint in hints
               while (search-forward cloze-string nil t)
               do
	       (when (and hint (not (string-empty-p hint)) (not (string= hint "nil"))
			  (not (string= "\"\"" hint))
			  (search-backward cloze-string nil t))
                 (replace-match (propertize (format "(%s)" hint)
					    'face 'gnosis-face-cloze))
                 (goto-char (match-end 0)))) ; Move point to end of match
      (buffer-string))))

(defun gnosis-cloze-mark-false (str answers)
  "Mark contents of STR as false for ANSWERS.

First item of answers will be marked as false, while the rest unanswered."
  (let* ((false (car answers))
	 (unanswered (cdr answers))
         (str-with-false (and answers
			      (gnosis-utils-highlight-words str (list false)
							 'gnosis-face-false)))
	 final)
    (if unanswered
	(setq final (gnosis-utils-highlight-words str-with-false
					       (if (listp unanswered) unanswered
						 (list unanswered))
					       'gnosis-face-unanswered))
      (setq final (or str-with-false str)))
    final))

(cl-defun gnosis--prompt (prompt &optional (downcase nil) (split nil))
  "PROMPT user for input until `q' is given.

The user is prompted to provide input for the PROMPT message.
Returns the list of non-q inputs in reverse order of their entry.

Set DOWNCASE to t to downcase all input given.
Set SPLIT to t to split all input given."
  (cl-loop with input = nil
           for response = (read-string (concat prompt " (q for quit): "))
	   do (if downcase (setf response (downcase response)))
           for response-parts = (if split (split-string response " ") (list response))
           if (member "q" response-parts) return (nreverse input)
           do (cl-loop for part in response-parts
	               unless (string-empty-p part)
                       do (push part input))))


(cl-defun gnosis-toggle-suspend-themata (ids &optional suspend-value verification)
  "Suspend or unsuspend themata IDS.

When SUSPEND-VALUE is nil and IDS has one element, toggle that thema's
current value.  When SUSPEND-VALUE is 0 or 1, set all IDS to that value
explicitly (safe for bulk operations).

When VERIFICATION is non-nil, skips `y-or-n-p' prompt."
  (cl-assert (listp ids) nil "IDS value needs to be a list.")
  (let* ((items-num (length ids))
         (suspend-value
          (or suspend-value
              (if (= items-num 1)
                  (if (= (gnosis-get 'suspend 'review-log `(= id ,(car ids))) 1) 0 1)
                1)))
         (action (if (= suspend-value 1) "Suspend" "Unsuspend"))
         (verification
          (or verification
              (if (= items-num 1)
                  (y-or-n-p (format "%s thema? " action))
                (y-or-n-p (format "%s %d themata? " action items-num))))))
    (when verification
      (gnosis-sqlite-execute-batch (gnosis--ensure-db)
        "UPDATE review_log SET suspend = ? WHERE id IN (%s)"
        ids
        (list suspend-value)))))


(defun gnosis-generate-id (&optional length)
  "Generate a unique gnosis ID.

When `gnosis--id-cache' is bound, uses hash table lookup instead of DB query.

LENGTH: length of id, default to 18."
  ;; NOTE: length must not exceed 18; 19-digit+ values can overflow sqlite.
  (let* ((length (or length 18))
         (max-val (expt 10 length))
         (min-val (expt 10 (1- length)))
         (id (+ (random (- max-val min-val)) min-val))
	 (exists (if gnosis--id-cache
		     (gethash id gnosis--id-cache)
		   (gnosis-select 'id 'themata `(= id ,id) t))))
    (if exists
        (gnosis-generate-id length)
      (when gnosis--id-cache
        (puthash id t gnosis--id-cache))
      id)))

(defun gnosis-generate-ids (n &optional length)
  "Generate N unique gnosis IDs as a list.
Uses `gnosis--id-cache' for O(1) collision checking when bound."
  (let ((ids nil) (count 0))
    (while (< count n)
      (let* ((len (or length 18))
             (max-val (expt 10 len))
             (min-val (expt 10 (1- len)))
             (id (+ (random (- max-val min-val)) min-val))
             (exists (if gnosis--id-cache
                         (gethash id gnosis--id-cache)
                       (gnosis-select 'id 'themata `(= id ,id) t))))
        (unless exists
          (when gnosis--id-cache (puthash id t gnosis--id-cache))
          (push id ids)
          (cl-incf count))))
    (nreverse ids)))

(defun gnosis-mcq-answer (id)
  "Choose the correct answer, from mcq choices for question ID."
  (let ((choices (gnosis-get 'hypothesis 'themata `(= id ,id)))
	(history-add-new-input nil)) ;; Disable history
    (gnosis-completing-read "Answer: " choices)))

(defun gnosis-cloze-check (sentence clozes)
  "Return t if all CLOZES are found in SENTENCE."
  (cl-every (lambda (cloze)
              (string-match-p
               (regexp-quote
	        (gnosis-utils-trim-quotes cloze))
               sentence))
            clozes))
;; TODO: use a better name to indicate that it also removes hints from STRING.
(defun gnosis-cloze-remove-tags (string)
  "Replace cloze tags and hints in STRING.

Works with both single (:), double colons (::), single braces ({}) and
double braces ({{}}).

Also removes content after a double semicolon (::), which indicate a hint."
  (let* ((regex "{\\{1,2\\}c[0-9]+:\\{1,2\\}\\(.*?\\)\\(::[^{}]*\\)?}\\{1,2\\}")
         (result (replace-regexp-in-string regex "\\1" string)))
    result))

(defun gnosis-cloze-extract-contents (str)
  "Extract cloze contents for STR.

Return a list of cloze tag contents for STR, organized by cX-tag.

Valid cloze formats include:
\"This is an {c1:example}\"
\"This is an {{c1::example}}\""
  (let ((result-alist '())
        (start 0))
    (while (string-match "{\\{1,2\\}c\\([0-9]+\\)::?\\(.*?\\)}\\{1,2\\}" str start)
      (let* ((tag (match-string 1 str))
             (content (match-string 2 str)))
        (if (assoc tag result-alist)
            (push content (cdr (assoc tag result-alist)))
          (push (cons tag (list content)) result-alist))
        (setf start (match-end 0))))
    (mapcar (lambda (tag-group) (nreverse (cdr tag-group)))
	    (nreverse result-alist))))

(defun gnosis-cloze-extract-answers (nested-lst)
  "Extract cloze answers for string clozes inside the NESTED-LST.

This function should be used in combination with
`gnosis-cloze-extract-contents'."
  (mapcar (lambda (lst)
            (mapcar (lambda (str)
                      (replace-regexp-in-string "::\\(.*\\)" "" str))
                    lst))
          nested-lst))

(defun gnosis-cloze-extract-hints (nested-lst)
  "Extract cloze hints for string clozes inside the NESTED-LST.

This function should be used in combination with
`gnosis-cloze-extract-contents'."
  (mapcar (lambda (lst)
            (mapcar (lambda (str)
                      (when (string-match "::\\(.*\\)" str)
                        (match-string 1 str)))
                    lst))
          nested-lst))

(defun gnosis-compare-strings (str1 str2)
  "Compare STR1 and STR2, ignoring case and whitespace."
  (let* ((normalized-str1 (downcase
			   (replace-regexp-in-string "\\s-" ""
						     (gnosis-utils-trim-quotes str1))))
         (normalized-str2 (downcase
			   (replace-regexp-in-string "\\s-" ""
						     (gnosis-utils-trim-quotes str2))))
         (max-length (max (length normalized-str1) (length normalized-str2))))
    (if (> max-length gnosis-string-difference)
        (<= (string-distance normalized-str1 normalized-str2) gnosis-string-difference)
      (string= normalized-str1 normalized-str2))))

(defun gnosis--read-string-with-input-method (prompt answer)
  "Read string with PROMPT, activating input method matching ANSWER's script.
Activates the input method in the current buffer so `read-string' with
INHERIT-INPUT-METHOD propagates it into the minibuffer.  Restores the
previous state on exit."
  (let ((method (alist-get (gnosis-utils-detect-script answer)
                           gnosis-script-input-method-alist)))
    (if (not method)
        (read-string prompt)
      (activate-input-method method)
      (unwind-protect
          (read-string prompt nil nil nil t)
        (deactivate-input-method)))))

(defun gnosis-get-tags--unique ()
  "Return a list of unique strings for tags in `gnosis-db'."
  (mapcar #'car
	  (gnosis-sqlite-select (gnosis--ensure-db)
	    "SELECT DISTINCT tag FROM thema_tag")))

(defun gnosis-get-tags-for-ids (ids)
  "Return unique tags for thema IDS."
  (when ids
    (mapcar #'car
	    (gnosis-sqlite-select-batch (gnosis--ensure-db)
	      "SELECT DISTINCT tag FROM thema_tag WHERE thema_id IN (%s)"
	      ids))))

(defun gnosis-collect-tag-thema-ids (tags &optional ids)
  "Collect thema IDS for TAGS."
  (cl-assert (listp tags))
  (if (null tags) ids
    (gnosis-collect-tag-thema-ids (cdr tags)
                                 (append ids (gnosis-get-tag-themata (car tags))))))

(defun gnosis-get-tag-themata (tag)
  "Return thema ids for TAG."
  (gnosis-select 'thema-id 'thema-tag `(= tag ,tag) t))

(defun gnosis-suspended-p (id)
  "Return t if thema with ID is suspended."
  (= (gnosis-get 'suspend 'review-log `(= id ,id)) 1))

(defun gnosis-filter-by-tags (include-tags exclude-tags)
  "Return thema IDs matching INCLUDE-TAGS but not EXCLUDE-TAGS.
When INCLUDE-TAGS is nil, start from all thema IDs."
  (let ((ids (if include-tags
		(cl-remove-duplicates (gnosis-collect-tag-thema-ids include-tags))
	      (gnosis-select 'id 'themata nil t))))
    (if exclude-tags
	(let ((excluded (cl-remove-duplicates
			 (gnosis-collect-tag-thema-ids exclude-tags))))
	  (cl-set-difference ids excluded))
      ids)))

(defun gnosis--date-to-int (date)
  "Convert DATE list (year month day) to YYYYMMDD integer for fast comparison."
  (+ (* (nth 0 date) 10000) (* (nth 1 date) 100) (nth 2 date)))

(defun gnosis--int-to-date (int)
  "Convert YYYYMMDD integer INT to (year month day) list."
  (list (/ int 10000) (% (/ int 100) 100) (% int 100)))

(defun gnosis--today-int ()
  "Return today as a YYYYMMDD integer.
Respects `gnosis-algorithm-day-start-hour'."
  (gnosis--date-to-int (gnosis-algorithm-date)))

(defun gnosis-tags--parse-filter (input)
  "Parse INPUT list of \"+tag\" / \"-tag\" strings.
Return (INCLUDE . EXCLUDE) cons of plain tag lists."
  (let (include exclude)
    (dolist (entry input)
      (cond ((string-prefix-p "+" entry)
	     (push (substring entry 1) include))
	    ((string-prefix-p "-" entry)
	     (push (substring entry 1) exclude))))
    (cons (nreverse include) (nreverse exclude))))

(defun gnosis-tags-filter-prompt (&optional tags)
  "Prompt for tag filters using +include / -exclude notation.
TAGS is an optional list of tag strings; defaults to all unique tags.
Return (INCLUDE . EXCLUDE) cons of plain tag lists."
  (interactive)
  (let* ((tags (or tags (gnosis-get-tags--unique)))
	 (candidates (cl-loop for tag in tags
			      nconc (list (concat "+" tag)
					  (concat "-" tag))))
	 (input (completing-read-multiple
		 "Filter tags (+include -exclude): "
		 candidates nil nil)))
    (gnosis-tags--parse-filter input)))

(defun gnosis-tags-prompt ()
  "Tag prompt for adding themata."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "This function is meant to be used in an org-mode buffer"))
  (save-excursion
    (let ((input (delete-dups
		  (completing-read-multiple
		   "Tags (separated by ,): " (gnosis-get-tags--unique))))
	  (current-tags (org-get-tags)))
      (outline-up-heading 99)
      (when input
	(setf gnosis-previous-thema-tags input)
        (org-set-tags (append input current-tags))))))

(defun gnosis-tag-rename (tag &optional new-tag)
  "Rename TAG to NEW-TAG, merging if NEW-TAG already exists.

Replace dashes (-) to underscores (_) for NEW-TAG, as org currently
does not accept heading tags with dashes.
When a thema already has NEW-TAG, the duplicate OLD row is removed."
  (let ((new-tag (or new-tag
		     (replace-regexp-in-string
		      "-" "_" (read-string "New tag name: "))))
	(db (gnosis--ensure-db)))
    (when (string-empty-p new-tag)
      (user-error "Tag name cannot be empty"))
    (when (string= tag new-tag)
      (user-error "New tag name is the same as the old one"))
    (gnosis-sqlite-with-transaction db
      ;; Remove rows where the thema already has new-tag (avoid UNIQUE conflict)
      (gnosis-sqlite-execute db
	"DELETE FROM thema_tag WHERE tag = ? AND thema_id IN
         (SELECT thema_id FROM thema_tag WHERE tag = ?)"
	(list tag new-tag))
      ;; Rename remaining rows
      (gnosis-sqlite-execute db
	"UPDATE thema_tag SET tag = ? WHERE tag = ?" (list new-tag tag)))
    (message "Renamed tag '%s' to '%s'" tag new-tag)))

(defun gnosis--tag-rename-batch (pairs)
  "Rename tags per PAIRS, an alist of (OLD . NEW).
Pairs where NEW is empty delete those tag rows instead of renaming.
Uses a temp mapping table so the entire rename is O(1) SQL statements."
  (let* ((delete-tags (mapcar #'car (cl-remove-if-not
				     (lambda (p) (string-empty-p (cdr p)))
				     pairs)))
	 (rename-pairs (cl-remove-if (lambda (p) (string-empty-p (cdr p)))
				     pairs))
	 (db (gnosis--ensure-db))
	 (max-vars (gnosis-sqlite--max-variable-number db)))
    (gnosis-sqlite-with-transaction db
      ;; Delete tags that rename to empty string
      (when delete-tags
	(gnosis-sqlite-execute-batch db
	  "DELETE FROM thema_tag WHERE tag IN (%s)" delete-tags))
      (when rename-pairs
	(gnosis-sqlite-execute db
	  "CREATE TEMP TABLE _tag_map (old_tag TEXT, new_tag TEXT)")
	;; Bulk-insert pairs, chunked to stay within variable limits
	(let ((offset 0)
	      (total (length rename-pairs)))
	  (while (< offset total)
	    (let* ((batch-size (/ max-vars 2))
		   (end (min (+ offset batch-size) total))
		   (chunk (cl-subseq rename-pairs offset end))
		   (placeholders (mapconcat (lambda (_) "(?, ?)") chunk ", "))
		   (params (cl-loop for (old . new) in chunk
				    append (list old new))))
	      (gnosis-sqlite-execute db
		(format "INSERT INTO _tag_map VALUES %s" placeholders)
		params)
	      (setq offset end))))
      ;; 1) Delete rows where thema already has the target tag
      (gnosis-sqlite-execute db
	"DELETE FROM thema_tag WHERE rowid IN (
           SELECT tt.rowid FROM thema_tag tt
           JOIN _tag_map m ON tt.tag = m.old_tag
           WHERE EXISTS (SELECT 1 FROM thema_tag t2
                         WHERE t2.thema_id = tt.thema_id
                           AND t2.tag = m.new_tag))")
      ;; 2) Delete intra-rename duplicates: multiple old tags -> same new tag
      ;;    for the same thema.  Keep the row with the smallest rowid.
      (gnosis-sqlite-execute db
	"DELETE FROM thema_tag WHERE rowid IN (
           SELECT tt.rowid FROM thema_tag tt
           JOIN _tag_map m ON tt.tag = m.old_tag
           WHERE EXISTS (SELECT 1 FROM thema_tag tt2
                         JOIN _tag_map m2 ON tt2.tag = m2.old_tag
                         WHERE tt2.thema_id = tt.thema_id
                           AND m2.new_tag = m.new_tag
                           AND tt2.rowid < tt.rowid))")
      ;; 3) Rename remaining rows
      (gnosis-sqlite-execute db
	"UPDATE thema_tag SET tag = (
           SELECT m.new_tag FROM _tag_map m WHERE m.old_tag = thema_tag.tag)
         WHERE tag IN (SELECT old_tag FROM _tag_map)")
	(gnosis-sqlite-execute db "DROP TABLE _tag_map")))))

(defun gnosis-modify-thema-tags (ids add-tags remove-tags)
  "Modify tags for thema IDS.  Add ADD-TAGS and remove REMOVE-TAGS.
Batched to stay within SQL variable limits."
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-with-transaction db
      (dolist (tag remove-tags)
        (gnosis-sqlite-execute-batch db
          "DELETE FROM thema_tag WHERE tag = ? AND thema_id IN (%s)"
          ids
          (list tag)))
      (dolist (tag add-tags)
        (let* ((max-vars (gnosis-sqlite--max-variable-number db))
               (batch-size (/ max-vars 2))
               (encoded-tag (prin1-to-string tag))
               (offset 0)
               (total (length ids)))
          (while (< offset total)
            (let* ((end (min (+ offset batch-size) total))
                   (chunk (cl-subseq ids offset end))
                   (placeholders (mapconcat (lambda (_) "(?, ?)") chunk ", "))
                   (params (cl-loop for id in chunk
                                    append (list id encoded-tag))))
              (sqlite-execute db
                (format "INSERT OR IGNORE INTO thema_tag (thema_id, tag) VALUES %s"
                        placeholders)
                params)
              (setq offset end))))))))

;; Links
(defun gnosis-extract-id-links (input &optional start)
  "Extract all link IDs from INPUT string and return them as a list.

START is the search starting position, used internally for recursion."
  (let ((start (or start 0)))
    (if (string-match "\\[\\[id:\\([^]]+\\)\\]\\[" input start)
        (cons (match-string 1 input)
              (gnosis-extract-id-links input (match-end 0)))
      nil)))

(cl-defun gnosis-collect-thema-ids (&key tags due query)
  "Return list of thema IDs filtered by TAGS, DUE, QUERY.

TAGS: cons (INCLUDE-TAGS . EXCLUDE-TAGS) as returned by
      `gnosis-tags-filter-prompt', or nil for no tag filtering.
DUE: non-nil to keep only due themata.
QUERY: search string."
  (let ((ids (cond (query (gnosis-search-thema query))
		   (tags  (gnosis-filter-by-tags (car tags) (cdr tags)))
		   (t     (gnosis-select 'id 'themata nil t)))))
    (if due
	(seq-intersection ids (gnosis-review-get-due-themata))
      ids)))


(defun gnosis-get-themata-by-reviews (max-reviews &optional thema-ids)
  "Return thema IDs with at most MAX-REVIEWS total reviews.
When THEMA-IDS is non-nil, restrict to that subset."
  (gnosis-select 'id 'review-log
                 (if thema-ids
                     `(and (<= n ,max-reviews)
                           (in id ,(vconcat thema-ids)))
                   `(<= n ,max-reviews))
                 t))


(defun gnosis-add-thema-fields (type keimenon hypothesis answer
				       parathema tags suspend links
				       &optional review-image gnosis-id)
  "Insert fields for new thema.

TYPE: Thema type e.g \"mcq\"
KEIMENON: Thema's keimenon
HYPOTHESIS: Thema hypothesis, e.g choices for mcq for OR hints for
cloze/basic thema
ANSWER: Correct answer for thema, for MCQ is an integer while for
cloze/basic a string/list of the right answer(s)
PARATHEMA: Parathema information to display after the answer
TAGS: Tags to organize themata
SUSPEND: Integer value of 1 or 0, where 1 suspends the card.
LINKS: List of id links."
  (cl-assert (stringp type) nil "Type must be a string")
  (cl-assert (stringp keimenon) nil "Keimenon must be a string")
  (cl-assert (listp hypothesis) nil "Hypothesis value must be a list")
  (cl-assert (listp answer) nil "Answer value must be a list")
  (cl-assert (stringp parathema) nil "Parathema must be a string")
  (cl-assert (listp tags) nil "Tags must be a list")
  (cl-assert (listp links) nil "Links must be a list")
  (let* ((gnosis-id (or gnosis-id (gnosis-generate-id)))
	 (review-image (or review-image "")))
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (gnosis--insert-into 'themata `([,gnosis-id ,(downcase type) ,keimenon ,hypothesis
					      ,answer nil]))
      (gnosis--insert-into 'review  `([,gnosis-id ,gnosis-algorithm-gnosis-value
						,gnosis-algorithm-amnesia-value]))
      (gnosis--insert-into 'review-log `([,gnosis-id ,(gnosis--today-int)
						   ,(gnosis--today-int) 0 0 0 0
						   ,suspend 0]))
      (gnosis--insert-into 'extras `([,gnosis-id ,parathema ,review-image]))
      (cl-loop for link in links
	       do (gnosis--insert-into 'thema-links `([,gnosis-id ,link])))
      (cl-loop for tag in tags
	       do (gnosis--insert-into 'thema-tag `([,gnosis-id ,tag]))))))

(defun gnosis-update-thema (id keimenon hypothesis answer parathema tags links
			      &optional type)
  "Update thema entry for ID.

If gnosis ID does not exist, create it anew.
When `gnosis--id-cache' is bound, uses hash table for existence check."
  (let* ((id (if (stringp id) (string-to-number id) id))
	 (current-type (gnosis-get 'type 'themata `(= id ,id))))
    (if (if gnosis--id-cache
	    (gethash id gnosis--id-cache)
	  (member id (gnosis-select 'id 'themata nil t)))
	(gnosis-sqlite-with-transaction (gnosis--ensure-db)
	  ;; Single multi-column UPDATE for themata
	  (gnosis-sqlite-execute (gnosis--ensure-db)
	    "UPDATE themata SET keimenon = ?, hypothesis = ?, answer = ?, type = ? WHERE id = ?"
	    (list keimenon hypothesis answer
		  (or type current-type) id))
	  ;; Single UPDATE for extras
	  (gnosis-update 'extras `(= parathema ,parathema) `(= id ,id))
	  ;; Re-sync links
	  (gnosis--delete 'thema-links `(= source ,id))
	  (cl-loop for link in links
		   do (gnosis--insert-into 'thema-links `([,id ,link])))
	  ;; Re-sync tags
	  (gnosis--delete 'thema-tag `(= thema-id ,id))
	  (cl-loop for tag in tags
		   do (gnosis--insert-into 'thema-tag `([,id ,tag]))))
      (message "Gnosis with id: %d does not exist, creating anew." id)
      (gnosis-add-thema-fields type keimenon hypothesis answer parathema tags
			      0 links nil id))))

;;;;;;;;;;;;;;;;;;;;;; THEMA HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These functions provide assertions depending on the type of thema.
;;
;; Each thema should use a helper function that calls to provide
;; assertions, such as length of hypothesis and answer, for said
;; thema.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gnosis-add-thema--assert-common (keimenon tags suspend links)
  "Assert common fields shared by all thema types.
KEIMENON, TAGS, SUSPEND, and LINKS are validated."
  (cl-assert (stringp keimenon) nil "Keimenon must be a string.")
  (cl-assert (and (listp tags) (cl-every #'stringp tags)) nil "Tags must be a list of strings.")
  (cl-assert (memq suspend '(0 1)) nil "Suspend value must be 0 or 1.")
  (cl-assert (and (listp links) (cl-every #'stringp links)) nil "Links must be a list of strings."))

(defun gnosis-add-thema--dispatch (id type keimenon hypothesis
				      answer parathema tags suspend links)
  "Dispatch thema creation or update for ID.
When ID is \"NEW\", create via `gnosis-add-thema-fields'.
Otherwise, update via `gnosis-update-thema'."
  (if (equal id "NEW")
      (gnosis-add-thema-fields type keimenon (or hypothesis (list ""))
			       answer parathema tags suspend links)
    (gnosis-update-thema id keimenon hypothesis answer parathema tags links type)))

(defun gnosis-add-thema--basic (id type keimenon hypothesis
				   answer parathema tags suspend links)
  "Add or update a basic thema."
  (gnosis-add-thema--assert-common keimenon tags suspend links)
  (cl-assert (or (null hypothesis)
		 (and (listp hypothesis) (= (length hypothesis) 1)))
	     nil "Hypothesis must be a list of a single item or nil.")
  (cl-assert (and (listp answer) (= (length answer) 1))
	     nil "Answer must be a list of a single item.")
  (gnosis-add-thema--dispatch id type keimenon hypothesis
			      answer parathema tags suspend links))

(defun gnosis-add-thema--double (id type keimenon hypothesis
				    answer parathema tags suspend links)
  "Add a double thema (two basic themata with reversed Q/A)."
  (gnosis-add-thema--assert-common keimenon tags suspend links)
  (cl-assert (listp hypothesis) nil "Hypothesis must be a list.")
  (cl-assert (and (listp answer) (= (length answer) 1))
	     nil "Answer must be a list of a single item.")
  (let ((type "basic")
	(hypothesis (or hypothesis (list ""))))
    (if (equal id "NEW")
	(progn
	  (gnosis-add-thema-fields type keimenon hypothesis
				   answer parathema tags suspend links)
	  (gnosis-add-thema-fields type (car answer) hypothesis
				   (list keimenon) parathema tags suspend links))
      (gnosis-update-thema id keimenon hypothesis answer parathema tags links type))))

(defun gnosis-add-thema--mcq (id type keimenon hypothesis
				answer parathema tags suspend links)
  "Add or update an MCQ thema."
  (gnosis-add-thema--assert-common keimenon tags suspend links)
  (cl-assert (string= type "mcq") nil "TYPE must be \"mcq\".")
  (cl-assert (and (listp hypothesis) (> (length hypothesis) 1))
	     nil "Hypothesis must be a list of more than 1 item.")
  (cl-assert (and (listp answer) (= (length answer) 1)
		  (member (car answer) hypothesis))
	     nil "Answer must be a single item, member of hypothesis.")
  (gnosis-add-thema--dispatch id type keimenon hypothesis
			      answer parathema tags suspend links))

(defun gnosis-add-thema--cloze (id type keimenon hypothesis
				  answer parathema tags suspend links)
  "Add or update a cloze thema."
  (gnosis-add-thema--assert-common keimenon tags suspend links)
  (cl-assert (string= type "cloze") nil "TYPE must be \"cloze\".")
  (cl-assert (or (null hypothesis) (>= (length answer) (length hypothesis)))
	     nil "Hypothesis length must not exceed answer length.")
  (cl-assert (listp answer) nil "Answer must be a list.")
  (cl-assert (gnosis-cloze-check keimenon answer) nil
	     "Cloze answers are not part of keimenon.")
  (let ((keimenon-clean (gnosis-cloze-remove-tags keimenon)))
    (if (equal id "NEW")
	(if (null answer)
	    (let* ((contents (gnosis-cloze-extract-contents keimenon))
		   (clozes (gnosis-cloze-extract-answers contents))
		   (hints (gnosis-cloze-extract-hints contents)))
	      (cl-loop for cloze in clozes
		       for hint in hints
		       do (gnosis-add-thema-fields type keimenon-clean hint cloze
						   parathema tags suspend links)))
	  (gnosis-add-thema-fields type keimenon-clean (or hypothesis (list ""))
				   answer parathema tags suspend links))
      (gnosis-update-thema id keimenon-clean hypothesis answer parathema tags links type))))

(defun gnosis-add-thema--mc-cloze (id type keimenon hypothesis
				  answer parathema tags suspend links)
  "Add or update an mc-cloze thema."
  (gnosis-add-thema--assert-common keimenon tags suspend links)
  (cl-assert (string= type "mc-cloze") nil "TYPE must be \"mc-cloze\".")
  (cl-assert (and (listp hypothesis) (> (length hypothesis) (length answer)))
	     nil "Hypothesis must be a list, longer than answer.")
  (cl-assert (and (listp answer) (length= answer 1)
		  (member (car answer) hypothesis))
	     nil "Answer must be a list of one item, member of hypothesis.")
  (cl-assert (gnosis-cloze-check keimenon answer) nil
	     "Cloze answers are not part of keimenon.")
  (let ((keimenon-clean (gnosis-cloze-remove-tags keimenon)))
    (gnosis-add-thema--dispatch id type keimenon-clean hypothesis
				answer parathema tags suspend links)))

;;;###autoload
(defun gnosis-add-thema (type &optional keimenon hypothesis
			      answer parathema tags example)
  "Add thema with TYPE."
  (interactive (list
		(downcase (completing-read "Select type: " gnosis-thema-types))))
  (window-configuration-to-register :gnosis-edit)
  (pop-to-buffer "*Gnosis NEW*")
  (with-current-buffer "*Gnosis NEW*"
    (let ((inhibit-read-only 1))
      (erase-buffer))
    (gnosis-edit-mode)
    (gnosis-export--insert-thema "NEW" type keimenon hypothesis
				answer parathema tags example))
  (search-backward "keimenon")
  (forward-line))

(defun gnosis-edit-thema (id)
  "Edit thema with ID."
  (window-configuration-to-register :gnosis-edit)
  (with-current-buffer (pop-to-buffer "*Gnosis Edit*")
    (let ((inhibit-read-only 1))
      (erase-buffer))
    (gnosis-edit-mode)
    (gnosis-export--insert-themata (list id))
    (search-backward "keimenon")
    (forward-line)))

(defun gnosis-edit-quit ()
  "Quit recrusive edit & kill current buffer."
  (interactive nil gnosis-edit-mode)
  (kill-buffer)
  (jump-to-register :gnosis-edit)
  (when gnosis-review-editing-p
    (setf gnosis-review-editing-p nil)
    (exit-recursive-edit)))

(defvar-keymap gnosis-edit-mode-map
  :doc "gnosis org mode map"
  "C-c C-c" #'gnosis-save
  "C-c C-q" #'gnosis-tags-prompt
  "C-c C-o" #'gnosis-nodes-goto-id
  "C-c C-k" #'gnosis-edit-quit)

(define-derived-mode gnosis-edit-mode org-mode "Gnosis Org"
  "Gnosis Org Mode."
  :interactive nil
  :lighter " Gnosis Edit"
  :keymap gnosis-edit-mode-map
  (setq header-line-format
	(substitute-command-keys
	 " Save thema by running \\[gnosis-save] or \\[gnosis-edit-quit] to quit")))

(defun gnosis-validate-custom-values (new-value)
  "Validate the structure and values of NEW-VALUE for gnosis-custom-values."
  (unless (listp new-value)
    (error "GNOSIS-CUSTOM-VALUES should be a list of entries"))
  (dolist (entry new-value)
    (unless (and (listp entry) (= (length entry) 3)
                 (eq (nth 0 entry) :tag)
                 (stringp (nth 1 entry))
                 (listp (nth 2 entry))) ; Ensure the third element is a plist
      (error
       "Each entry should have a :tag keyword, a string, and a plist of custom values"))
    (let ((proto (plist-get (nth 2 entry) :proto))
          (anagnosis (plist-get (nth 2 entry) :anagnosis))
          (epignosis (plist-get (nth 2 entry) :epignosis))
          (agnoia (plist-get (nth 2 entry) :agnoia))
          (amnesia (plist-get (nth 2 entry) :amnesia))
          (lethe (plist-get (nth 2 entry) :lethe)))
      (unless (and (listp proto) (cl-every #'integerp proto))
        (error "Proto must be a list of integer values"))
      (unless (or (null anagnosis) (integerp anagnosis))
        (error "Anagnosis should be an integer"))
      (unless (or (null epignosis) (numberp epignosis))
        (error "Epignosis should be a number"))
      (unless (or (null agnoia) (numberp agnoia))
        (error "Agnoia should be a number"))
      (unless (or (null amnesia) (and (numberp amnesia) (<= amnesia 1) (>= amnesia 0)))
        (error "Amnesia should be a number between 0 and 1"))
      (unless (or (null lethe) (and (integerp lethe) (> lethe 0)))
        (error "Lethe should be an integer greater than 0")))))

(defvar gnosis--custom-values-ht nil
  "Hash table cache mapping tag strings to their custom value plists.
Built lazily by `gnosis--custom-values-lookup', cleared by the
`gnosis-custom-values' watcher.")

(defun gnosis--build-custom-values-ht (&optional values)
  "Build hash table from VALUES (defaults to `gnosis-custom-values').
Each tag key maps to the merged plist of all matching rules."
  (let ((ht (make-hash-table :test #'equal))
        (rules (or values gnosis-custom-values)))
    (dolist (rule rules)
      (let ((tag (plist-get rule :tag)))
        (when (stringp tag)
          (let ((existing (gethash tag ht))
                (props (nth 2 rule)))
            (puthash tag (append existing props) ht)))))
    ht))

(defun gnosis--custom-values-lookup (tag &optional values)
  "Look up custom values for TAG, using cache when possible.
When VALUES is non-nil (test path), bypasses the cache and
searches VALUES directly."
  (if values
      (gnosis-get-custom-values :tag tag values)
    (unless gnosis--custom-values-ht
      (setq gnosis--custom-values-ht (gnosis--build-custom-values-ht)))
    (let ((plist (gethash tag gnosis--custom-values-ht)))
      (when plist
        (gnosis-get-custom-values--validate plist gnosis-custom--valid-values))
      plist)))

(defun gnosis-custom-values-watcher (symbol new-value _operation _where)
  "Watcher for gnosis custom values.

SYMBOL to watch changes for.
NEW-VALUE is the new value set to the variable.
OPERATION is the type of operation being performed.
WHERE is the buffer or object where the change happens."
  (when (eq symbol 'gnosis-custom-values)
    (setq gnosis--custom-values-ht nil)
    (gnosis-validate-custom-values new-value)))

(add-variable-watcher 'gnosis-custom-values 'gnosis-custom-values-watcher)

;; Validate custom values during review process as well.
(defun gnosis-get-custom-values--validate (plist valid-keywords)
  "Verify that PLIST consists of VALID-KEYWORDS."
  (let ((keys (let (ks)
                (while plist
                  (setq ks (cons (car plist) ks))
                  (setq plist (cddr plist)))
                ks)))
    (let ((invalid-key (cl-find-if (lambda (key) (not (member key valid-keywords))) keys)))
      (if invalid-key
          (error "Invalid custom keyword found in: %s" invalid-key)
        t))))

(defun gnosis-get-custom-values (key search-value &optional values)
  "Return SEARCH-VALUE for KEY from VALUES.

VALUES: Defaults to `gnosis-custom-values'."
  (cl-assert (eq key :tag) nil "Key value must be :tag")
  (cl-assert (stringp search-value) nil "Search-value must be the name of a tag as a string.")
  (let ((results)
	(values (or values gnosis-custom-values)))
    (dolist (rule values)
      (when (and (plist-get rule key)
                 (equal (plist-get rule key) search-value))
        (setq results (append results (nth 2 rule)))))
    (gnosis-get-custom-values--validate results gnosis-custom--valid-values)
    results))

(defun gnosis-get-custom-tag-values (id keyword &optional custom-tags custom-values)
  "Return KEYWORD values for thema ID.
Uses cached hash table lookup when CUSTOM-VALUES is nil."
  (cl-assert (keywordp keyword) nil "keyword must be a keyword!")
  (let ((tags (if id (gnosis-select 'tag 'thema-tag `(= thema-id ,id) t) custom-tags)))
    (cl-loop for tag in tags
	     for val = (plist-get (gnosis--custom-values-lookup tag custom-values) keyword)
	     when val collect val)))

(defun gnosis--get-tag-value (id keyword aggregator &optional custom-tags custom-values)
  "Return aggregated tag value for thema ID and KEYWORD.

AGGREGATOR combines multiple tag values (e.g., #\\='max or #\\='min).
Returns nil when no tags define KEYWORD."
  (let ((vals (gnosis-get-custom-tag-values id keyword custom-tags custom-values)))
    (and vals (apply aggregator vals))))

(defun gnosis-get-thema-custom-value (id keyword aggregator default-var
					 &optional validate-p custom-tags custom-values)
  "Return the custom algorithm value for thema ID and KEYWORD.

Looks up tag values (aggregated with AGGREGATOR), falling back to
DEFAULT-VAR.
When VALIDATE-P is non-nil, signals error if value >= 1."
  (let* ((tag-val (gnosis--get-tag-value id keyword aggregator custom-tags custom-values))
         (val (or tag-val default-var)))
    (when (and validate-p (>= val 1))
      (error "%s value must be lower than 1" keyword))
    val))

;; Named wrappers -- tag variants (used in tests)

(defun gnosis-get-thema-tag-amnesia (id &optional custom-tags custom-values)
  "Return tag amnesia for thema ID."
  (gnosis--get-tag-value id :amnesia #'max custom-tags custom-values))

(defun gnosis-get-thema-tag-epignosis (id &optional custom-tags custom-values)
  "Return tag epignosis for thema ID."
  (gnosis--get-tag-value id :epignosis #'max custom-tags custom-values))

(defun gnosis-get-thema-tag-agnoia (id &optional custom-tags custom-values)
  "Return tag agnoia for thema ID."
  (gnosis--get-tag-value id :agnoia #'max custom-tags custom-values))

(defun gnosis-get-thema-tag-anagnosis (id &optional custom-tags custom-values)
  "Return tag anagnosis for thema ID."
  (gnosis--get-tag-value id :anagnosis #'min custom-tags custom-values))

(defun gnosis-get-thema-tag-lethe (id &optional custom-tags custom-values)
  "Return tag lethe for thema ID."
  (gnosis--get-tag-value id :lethe #'min custom-tags custom-values))

;; Named wrappers -- merged (tag overrides default)

(defun gnosis-get-thema-amnesia (id &optional custom-tags custom-values)
  "Return amnesia value for thema ID."
  (gnosis-get-thema-custom-value id :amnesia #'max gnosis-algorithm-amnesia-value
				 t custom-tags custom-values))

(defun gnosis-get-thema-epignosis (id &optional custom-tags custom-values)
  "Return epignosis value for thema ID."
  (gnosis-get-thema-custom-value id :epignosis #'max gnosis-algorithm-epignosis-value
				 t custom-tags custom-values))

(defun gnosis-get-thema-agnoia (id &optional custom-tags custom-values)
  "Return agnoia value for thema ID."
  (gnosis-get-thema-custom-value id :agnoia #'max gnosis-algorithm-agnoia-value
				 t custom-tags custom-values))

(defun gnosis-proto-max-values (proto-values)
  "Return max values from PROTO-VALUES."
  (if (not (and (listp proto-values) (cl-every #'listp proto-values)))
      proto-values
    (let* ((max-len (apply #'max (mapcar #'length proto-values)))
           (padded-lists (mapcar (lambda (lst)
                                   (append lst (make-list (- max-len (length lst)) 0)))
                                 proto-values)))
      (apply #'cl-mapcar #'max padded-lists))))

(defun gnosis-get-thema-proto (id &optional custom-tags custom-values)
  "Return proto values for thema ID.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-TAGS: Custom tags to be used instead."
  (let ((tags-proto (gnosis-get-custom-tag-values id :proto custom-tags custom-values)))
    (if tags-proto (gnosis-proto-max-values tags-proto)
      gnosis-algorithm-proto)))

(defun gnosis-get-thema-anagnosis (id &optional custom-tags custom-values)
  "Return anagnosis value for thema ID."
  (gnosis-get-thema-custom-value id :anagnosis #'min gnosis-algorithm-anagnosis-value
				 nil custom-tags custom-values))

(defun gnosis-get-thema-lethe (id &optional custom-tags custom-values)
  "Return lethe value for thema ID."
  (gnosis-get-thema-custom-value id :lethe #'min gnosis-algorithm-lethe-value
				 nil custom-tags custom-values))

(defun gnosis-get-date-total-themata (&optional date)
  "Return total themata reviewed for DATE (YYYYMMDD integer).

If entry for DATE does not exist, it will be created.

Defaults to current date."
  (let* ((date (or date (gnosis--today-int)))
	 (date-log (gnosis-select
		    '[date reviewed-total reviewed-new] 'activity-log
		    `(= date ,date) t))
	 (reviewed-total (cadr date-log))
	 (reviewed-new (or (caddr date-log) 0)))
    (or reviewed-total
	(progn
	  ;; Using reviewed-new instead of hardcoding 0 just to not mess up tests.
	  (and (= date (gnosis--today-int))
	       (gnosis--insert-into 'activity-log `([,date 0 ,reviewed-new])))
	  0))))

(defun gnosis-get-date-new-themata (&optional date)
  "Return new themata reviewed for DATE (YYYYMMDD integer).

Defaults to current date."
  (let* ((date (or date (gnosis--today-int)))
	 (reviewed-new (or (car (gnosis-select 'reviewed-new 'activity-log `(= date ,date) t))
			   0)))
    reviewed-new))
(defun gnosis-search-thema (&optional query)
  "Search for thema QUERY.

Return thema ids for themata that match QUERY."
  (cl-assert (or (stringp query) (eq query nil)))
  (let* ((query (or query (read-string "Search for thema: ")))
         (words (split-string query))
         (clause-keimenon `(and ,@(mapcar (lambda (word)
					`(like keimenon ,(format "%%%s%%" word)))
                                      words)))
	 (clause-answer `(and ,@(mapcar (lambda (word)
					  `(like answer ,(format "%%%s%%" word)))
					words))))
    (append (gnosis-select 'id 'themata clause-keimenon t)
	    (gnosis-select 'id 'themata clause-answer t))))

;;; Database Schemas
(defconst gnosis-db--schemata
  '((themata
     ([(id integer :primary-key)
       (type text :not-null)
       (keimenon text :not-null)
       (hypothesis text :not-null)
       (answer text :not-null)
       (source-guid text)]))
    (review
     ([(id integer :primary-key :not-null) ;; thema-id
       (gnosis integer :not-null)
       (amnesia integer :not-null)]
      (:foreign-key [id] :references themata [id]
		    :on-delete :cascade)))
    (review-log
     ([(id integer :primary-key :not-null) ;; thema-id
       (last-rev integer :not-null)  ;; Last review date
       (next-rev integer :not-null)  ;; Next review date
       (c-success integer :not-null) ;; Consecutive successful reviews
       (t-success integer :not-null) ;; Total successful reviews
       (c-fails integer :not-null)   ;; Consecutive failed reviewss
       (t-fails integer :not-null)   ;; Total failed reviews
       (suspend integer :not-null)   ;; Binary value, 1=suspended
       (n integer :not-null)]        ;; Number of reviews
      (:foreign-key [id] :references themata [id]
		    :on-delete :cascade)))
    (activity-log
     ([(date integer :not-null)
       (reviewed-total integer :not-null)
       (reviewed-new integer :not-null)]))
    (extras
     ([(id integer :primary-key :not-null)
       (parathema string)
       (review-image string)]
      (:foreign-key [id] :references themata [id]
		    :on-delete :cascade)))
    (thema-tag
     ([(thema-id integer :not-null)
       (tag text :not-null)]
      (:foreign-key [thema-id] :references themata [id]
		    :on-delete :cascade)
      (:unique [thema-id tag])))
    (thema-links
     ([(source integer)
       (dest text)]
      (:foreign-key [source] :references themata [id]
		    :on-delete :cascade)
      (:unique [source dest])))
    ;; Node tables (merged from org-gnosis)
    (nodes
     ([(id text :not-null :primary-key)
       (file text :not-null)
       (title text :not-null)
       (level text :not-null)
       (tags text)
       (mtime text)
       (hash text)]))
    (journal
     ([(id text :not-null :primary-key)
       (file text :not-null)
       (title text :not-null)
       (level text :not-null)
       (tags text)
       (mtime text)
       (hash text)]))
    (node-tag
     ([(node-id text :not-null)
       (tag text :not-null)]
      (:foreign-key [node-id] :references nodes [id]
		    :on-delete :cascade)
      (:unique [node-id tag])))
    (node-links
     ([(source text)
       (dest text)]
      (:foreign-key [source] :references nodes [id]
		    :on-delete :cascade)
      (:unique [source dest])))))

(defun gnosis--db-version ()
  "Return the current user_version pragma from the database."
  (caar (gnosis-sqlite-select (gnosis--ensure-db) "PRAGMA user_version")))

(defun gnosis--db-set-version (version)
  "Set the database user_version pragma to VERSION."
  (gnosis-sqlite-execute (gnosis--ensure-db)
			 (format "PRAGMA user_version = %d" version)))

(defun gnosis--db-create-tables ()
  "Create all tables and set version to current.
Used for fresh databases only."
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-with-transaction db
      (pcase-dolist (`(,table ,schema) gnosis-db--schemata)
	(gnosis-sqlite-execute db
	  (format "CREATE TABLE %s (%s)"
		  (gnosis-sqlite--ident table)
		  (gnosis-sqlite--compile-schema schema))))
      (gnosis--db-create-indexes db)
      (gnosis--db-set-version gnosis-db-version))))

(defun gnosis--db-create-indexes (db)
  "Create all performance indexes on DB."
  (dolist (stmt '("CREATE INDEX IF NOT EXISTS idx_review_log_due
                   ON review_log(n, suspend, next_rev)"
		  "CREATE INDEX IF NOT EXISTS idx_thema_tag_thema_id
                   ON thema_tag(thema_id)"
		  "CREATE INDEX IF NOT EXISTS idx_thema_tag_tag
                   ON thema_tag(tag)"
		  "CREATE INDEX IF NOT EXISTS idx_thema_links_source
                   ON thema_links(source)"
		  "CREATE INDEX IF NOT EXISTS idx_thema_links_dest
                   ON thema_links(dest)"
		  "CREATE INDEX IF NOT EXISTS idx_node_links_source
                   ON node_links(source)"
		  "CREATE INDEX IF NOT EXISTS idx_node_links_dest
                   ON node_links(dest)"
		  "CREATE INDEX IF NOT EXISTS idx_activity_log_date
                   ON activity_log(date)"
		  "CREATE INDEX IF NOT EXISTS idx_nodes_file
                   ON nodes(file)"
		  "CREATE INDEX IF NOT EXISTS idx_journal_file
                   ON journal(file)"))
    (gnosis-sqlite-execute db stmt))
  ;; source_guid index: created by v8 migration for existing DBs,
  ;; or here for fresh DBs where the column already exists
  (ignore-errors
    (gnosis-sqlite-execute db
      "CREATE INDEX IF NOT EXISTS idx_themata_source_guid ON themata(source_guid)")))

(defun gnosis--db-has-tables-p ()
  "Return non-nil if the database has user tables."
  (let ((tables (gnosis-sqlite-select (gnosis--ensure-db)
				      "SELECT name FROM sqlite_master WHERE type = 'table'")))
    (length> tables 0)))

;;; Migration helpers

(defun gnosis--migrate-make-list (column)
  "Make COLUMN values into a list."
  (let ((col-name (gnosis-sqlite--ident column))
	(results (gnosis-select `[id ,column] 'themata)))
    (dolist (row results)
      (let ((id (car row))
            (old-value (cadr row)))
	(unless (listp old-value)
	  (gnosis-sqlite-execute (gnosis--ensure-db)
				 (format "UPDATE themata SET %s = ? WHERE id = ?" col-name)
				 (list (list old-value) id)))))))

(defun gnosis-db--migrate-v1 ()
  "Migration v1: rename notes table to themata."
  (gnosis-sqlite-execute (gnosis--ensure-db) "ALTER TABLE notes RENAME TO themata")
  (gnosis--db-set-version 1))

(defun gnosis--migrate-get-tags-from-column ()
  "Read unique tags from the serialized themata.tags column.
Used by migrations that run before the thema-tag junction table exists."
  (cl-loop for tags in (apply 'append
			      (gnosis-sqlite-select (gnosis--ensure-db)
						    "SELECT DISTINCT tags FROM themata"))
	   nconc tags into all-tags
	   finally return (delete-dups all-tags)))

(defun gnosis-db--migrate-v2 ()
  "Migration v2: add deck algorithm columns and activity log."
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-execute db "ALTER TABLE decks ADD COLUMN failure_factor FLOAT")
    (gnosis-sqlite-execute db "ALTER TABLE decks ADD COLUMN ef_increase FLOAT")
    (gnosis-sqlite-execute db "ALTER TABLE decks ADD COLUMN ef_decrease FLOAT")
    (gnosis-sqlite-execute db "ALTER TABLE decks ADD COLUMN ef_threshold INTEGER")
    (gnosis-sqlite-execute db "ALTER TABLE decks ADD COLUMN initial_interval TEXT")
    (gnosis-sqlite-execute db
      "CREATE TABLE IF NOT EXISTS activity_log (
         date TEXT NOT NULL, reviewed_total INTEGER NOT NULL, reviewed_new INTEGER NOT NULL)"))
  (gnosis--db-set-version 2))

(defun gnosis-db--migrate-v3 ()
  "Migration v3: drop deck columns, rename review columns."
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-execute db "ALTER TABLE decks DROP COLUMN failure_factor")
    (gnosis-sqlite-execute db "ALTER TABLE decks DROP COLUMN ef_increase")
    (gnosis-sqlite-execute db "ALTER TABLE decks DROP COLUMN ef_decrease")
    (gnosis-sqlite-execute db "ALTER TABLE decks DROP COLUMN ef_threshold")
    (gnosis-sqlite-execute db "ALTER TABLE decks DROP COLUMN initial_interval")
    (gnosis-sqlite-execute db "ALTER TABLE review RENAME COLUMN ef TO gnosis")
    (gnosis-sqlite-execute db "ALTER TABLE review RENAME COLUMN ff TO amnesia")
    (gnosis-sqlite-execute db "ALTER TABLE review DROP COLUMN interval")
    (gnosis-sqlite-execute db
      "CREATE TABLE IF NOT EXISTS activity_log (
         date TEXT NOT NULL, reviewed_total INTEGER NOT NULL, reviewed_new INTEGER NOT NULL)"))
  (gnosis--db-set-version 3))

(defun gnosis-db--migrate-v4 ()
  "Migration v4: column renames, tags/links tables, data conversions."
  (let ((db (gnosis--ensure-db)))
    ;; 1. Rename notes -> themata (no-op if v1 already did it)
    (ignore-errors
      (gnosis-sqlite-execute db "ALTER TABLE notes RENAME TO themata"))
    ;; 2. Create tags and links tables (v5 will rename them)
    (gnosis-sqlite-execute db
      "CREATE TABLE IF NOT EXISTS tags (tag text PRIMARY KEY, UNIQUE (tag))")
    (gnosis-sqlite-execute db
      "CREATE TABLE IF NOT EXISTS links (source integer, dest text,
         FOREIGN KEY (source) REFERENCES themata (id) ON DELETE CASCADE,
         UNIQUE (source, dest))")
    ;; 3. Populate tags from serialized themata.tags column
    (let ((tags (gnosis--migrate-get-tags-from-column)))
      (cl-loop for tag in tags
               do (gnosis-sqlite-execute db
                    "INSERT OR IGNORE INTO tags VALUES (?)" (list tag))))
    ;; 4. Column renames
    (gnosis-sqlite-execute db "ALTER TABLE themata RENAME COLUMN main TO keimenon")
    (gnosis-sqlite-execute db "ALTER TABLE themata RENAME COLUMN options TO hypothesis")
    (gnosis-sqlite-execute db "ALTER TABLE extras RENAME COLUMN extra_notes TO parathema")
    (gnosis-sqlite-execute db "ALTER TABLE extras RENAME COLUMN images TO review_image")
    (gnosis-sqlite-execute db "ALTER TABLE extras DROP COLUMN extra_image")
    ;; 5. Make sure all hypothesis & answer values are lists
    (gnosis--migrate-make-list 'hypothesis)
    (gnosis--migrate-make-list 'answer)
    ;; 6. Fix MCQ integer answers
    (cl-loop for thema in (gnosis-select 'id 'themata '(= type "mcq") t)
             do (let* ((data (gnosis-select '[hypothesis answer] 'themata
                              `(= id ,thema) t))
                       (hypothesis (nth 0 data))
                       (old-answer (car (nth 1 data)))
                       (new-answer (when (integerp old-answer)
                                     (list (nth (1- old-answer) hypothesis)))))
                  (when (integerp old-answer)
                    (gnosis-update 'themata `(= answer ',new-answer)
                                   `(= id ,thema)))))
    ;; 7. Replace y-or-n with MCQ
    (cl-loop for thema in (gnosis-select 'id 'themata '(= type "y-or-n") t)
             do (let ((data (gnosis-select '[type hypothesis answer]
                              'themata `(= id ,thema) t)))
                  (when (string= (nth 0 data) "y-or-n")
                    (gnosis-update 'themata '(= type "mcq") `(= id ,thema))
                    (gnosis-update 'themata '(= hypothesis '("Yes" "No"))
                                   `(= id ,thema))
                    (if (= (car (nth 2 data)) 121)
                        (gnosis-update 'themata '(= answer '("Yes"))
                                       `(= id ,thema))
                      (gnosis-update 'themata '(= answer '("No"))
                                     `(= id ,thema))))))
    ;; 8. Replace - with _ in tags (org does not support tags with dash)
    (cl-loop for tag in (gnosis--migrate-get-tags-from-column)
             if (string-match-p "-" tag)
             do (let ((new-tag (replace-regexp-in-string "-" "_" tag)))
                  (cl-loop for thema in (gnosis-select 'id 'themata
                                          `(like tags ',(format "%%\"%s\"%%" tag)) t)
                           do (let* ((tags-val (car (gnosis-select '[tags] 'themata `(= id ,thema) t)))
                                     (new-tags (cl-substitute new-tag tag tags-val :test #'string-equal)))
                                (gnosis-update 'themata `(= tags ',new-tags) `(= id ,thema))))
                  (gnosis-sqlite-execute db "DELETE FROM tags")
                  (cl-loop for tag-item in (gnosis--migrate-get-tags-from-column)
                           do (gnosis-sqlite-execute db "INSERT OR IGNORE INTO tags VALUES (?)"
                                                     (list tag-item))))))
  (gnosis--db-set-version 4))

(defun gnosis-db--migrate-v5 ()
  "Migration v5: rename notes table to themata."
  (ignore-errors
    (gnosis-sqlite-execute (gnosis--ensure-db) "ALTER TABLE notes RENAME TO themata"))
  (gnosis--db-set-version 5))

(defun gnosis-db--migrate-v6 ()
  "Migration v6: merge org-gnosis tables, rename links/tags."
  (let ((db (gnosis--ensure-db)))
    ;; 0. Move deck names into thema tags, then drop decks
    (ignore-errors
      (let ((rows (gnosis-sqlite-select db
                    "SELECT t.id, t.tags, d.name
                     FROM themata t JOIN decks d ON t.deck_id = d.id")))
        (dolist (row rows)
          (let* ((thema-id (nth 0 row))
                 (tags (nth 1 row))
                 (deck-name (nth 2 row))
                 (new-tags (if (listp tags)
                               (append tags (list deck-name))
                             (list tags deck-name))))
            (gnosis-sqlite-execute db
              "UPDATE themata SET tags = ? WHERE id = ?"
              (list new-tags thema-id))))))
    (ignore-errors
      (gnosis-sqlite-execute db "ALTER TABLE themata DROP COLUMN deck_id"))
    (ignore-errors
      (gnosis-sqlite-execute db "DROP TABLE IF EXISTS decks"))
    ;; 1. Rename existing tables (idempotent for PRAGMA 5 users)
    (ignore-errors
      (gnosis-sqlite-execute db "ALTER TABLE links RENAME TO thema_links"))
    (ignore-errors
      (gnosis-sqlite-execute db "ALTER TABLE tags RENAME TO thema_tags"))
    ;; 2. Create thema-tag junction table & populate from themata.tags
    (let ((schema (cadr (assq 'thema-tag gnosis-db--schemata))))
      (gnosis-sqlite-execute db
        (format "CREATE TABLE IF NOT EXISTS %s (%s)"
		(gnosis-sqlite--ident 'thema-tag)
		(gnosis-sqlite--compile-schema schema))))
    (let ((rows (gnosis-sqlite-select db "SELECT id, tags FROM themata")))
      (dolist (row rows)
	(let ((thema-id (car row))
	      (tags (cadr row)))
	  (when (listp tags)
	    (dolist (tag tags)
	      (ignore-errors
		(gnosis-sqlite-execute db
		  "INSERT OR IGNORE INTO thema_tag (thema_id, tag) VALUES (?, ?)"
		  (list thema-id tag))))))))
    ;; Drop serialized tags column (now replaced by thema-tag junction table)
    (ignore-errors
      (gnosis-sqlite-execute db "ALTER TABLE themata DROP COLUMN tags"))
    ;; Drop thema-tags and node-tags lookup tables
    (ignore-errors
      (gnosis-sqlite-execute db "DROP TABLE IF EXISTS thema_tags"))
    (ignore-errors
      (gnosis-sqlite-execute db "DROP TABLE IF EXISTS node_tags"))
    ;; 3. Create node tables
    (dolist (table '(nodes journal node-tag node-links))
      (let ((schema (cadr (assq table gnosis-db--schemata))))
        (gnosis-sqlite-execute db
          (format "CREATE TABLE IF NOT EXISTS %s (%s)"
                  (gnosis-sqlite--ident table)
                  (gnosis-sqlite--compile-schema schema)))))
    ;; 3. Create indexes
    (gnosis-sqlite-execute db
      "CREATE INDEX IF NOT EXISTS idx_nodes_file ON nodes (file)")
    (gnosis-sqlite-execute db
      "CREATE INDEX IF NOT EXISTS idx_journal_file ON journal (file)")
    ;; 4. Import from org-gnosis.db if it exists
    (let ((org-db-file (locate-user-emacs-file "org-gnosis.db")))
      (when (file-exists-p org-db-file)
        (gnosis-sqlite-execute db
          (format "ATTACH DATABASE '%s' AS org_gnosis"
                  (expand-file-name org-db-file)))
        (ignore-errors
          (gnosis-sqlite-execute db
            "INSERT OR IGNORE INTO nodes SELECT * FROM org_gnosis.nodes")
          (gnosis-sqlite-execute db
            "INSERT OR IGNORE INTO journal SELECT * FROM org_gnosis.journal")
          (gnosis-sqlite-execute db
            "INSERT OR IGNORE INTO node_tag SELECT * FROM org_gnosis.node_tag")
          (gnosis-sqlite-execute db
            "INSERT OR IGNORE INTO node_links SELECT * FROM org_gnosis.links"))
        (gnosis-sqlite-execute db "DETACH DATABASE org_gnosis")
        (message "Imported org-gnosis data into unified database"))))
  (gnosis--db-set-version 6))

(defun gnosis--migrate-date-to-int (value)
  "Convert VALUE to YYYYMMDD integer for migration.
Handles both Lisp list dates and already-converted integers."
  (cond
   ((integerp value) value)
   ((and (listp value) (= (length value) 3))
    (gnosis--date-to-int value))
   ((null value) nil)
   (t (warn "gnosis: unexpected date value during migration: %S" value)
      nil)))

(defun gnosis-db--migrate-v7 ()
  "Migration v7: convert date columns from Lisp lists to YYYYMMDD integers."
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-with-transaction db
      ;; 1. Convert review_log.last_rev and next_rev
      (dolist (row (gnosis-sqlite-select db
                     "SELECT id, last_rev, next_rev FROM review_log"))
        (let ((new-last (gnosis--migrate-date-to-int (nth 1 row)))
              (new-next (gnosis--migrate-date-to-int (nth 2 row))))
          (when (and new-last new-next
                     (or (not (equal (nth 1 row) new-last))
                         (not (equal (nth 2 row) new-next))))
            (gnosis-sqlite-execute db
              "UPDATE review_log SET last_rev = ?, next_rev = ? WHERE id = ?"
              (list new-last new-next (nth 0 row))))))
      ;; 2. Convert activity_log.date
      (dolist (row (gnosis-sqlite-select db
                     "SELECT rowid, date FROM activity_log"))
        (let ((new-date (gnosis--migrate-date-to-int (nth 1 row))))
          (when (and new-date (not (equal (nth 1 row) new-date)))
            (gnosis-sqlite-execute db
              "UPDATE activity_log SET date = ? WHERE rowid = ?"
              (list new-date (nth 0 row))))))
      ;; 3. Create indexes
      (gnosis--db-create-indexes db)))
  (gnosis--db-set-version 7))

(defun gnosis-db--migrate-v8 ()
  "Add source_guid column to themata for Anki GUID-based dedup."
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-execute db
      "ALTER TABLE themata ADD COLUMN source_guid TEXT")
    (gnosis-sqlite-execute db
      "CREATE INDEX IF NOT EXISTS idx_themata_source_guid ON themata(source_guid)"))
  (gnosis--db-set-version 8))

(defconst gnosis-db--migrations
  `((1 . gnosis-db--migrate-v1)
    (2 . gnosis-db--migrate-v2)
    (3 . gnosis-db--migrate-v3)
    (4 . gnosis-db--migrate-v4)
    (5 . gnosis-db--migrate-v5)
    (6 . gnosis-db--migrate-v6)
    (7 . gnosis-db--migrate-v7)
    (8 . gnosis-db--migrate-v8))
  "Alist of (VERSION . FUNCTION).
Each migration brings the DB from VERSION-1 to VERSION.")

(defun gnosis--db-run-migrations (current-version)
  "Run all pending migrations from CURRENT-VERSION to `gnosis-db-version'.
Commits the database after all migrations complete."
  (let ((migrated nil))
    (cl-loop for (version . func) in gnosis-db--migrations
	     when (> version current-version)
	     do (progn
		  (message "Gnosis: running migration to v%d..." version)
		  (funcall func)
		  (message "Gnosis: migration to v%d complete" version)
		  (setq migrated version)))
    (when migrated
      (gnosis--commit-migration current-version migrated))))

(defun gnosis--commit-migration (from to)
  "Commit database after migrating from version FROM to TO.
Uses synchronous git operations because migration must complete
before database initialization continues."
  (let ((default-directory gnosis-dir))
    (unless gnosis-testing
      (when (file-exists-p (expand-file-name ".git" gnosis-dir))
        (call-process (executable-find "git") nil nil nil "add" "gnosis.db")
        (call-process (executable-find "git") nil nil nil
                      "commit" "-m"
                      (format "Migrate database v%d -> v%d" from to))))))

(defun gnosis-db-init ()
  "Initialize database: create tables if fresh, run pending migrations."
  (let ((version (gnosis--db-version)))
    (if (and (zerop version) (not (gnosis--db-has-tables-p)))
	;; Fresh database: create all tables at current version
	(gnosis--db-create-tables)
      ;; Existing database: run any pending migrations
      (gnosis--db-run-migrations version))))

;; VC functions ;;
;;;;;;;;;;;;;;;;;;

(defun gnosis--git-cmd (args &optional sentinel)
  "Run git with ARGS list, watching for password prompts.

ARGS is a list of strings passed directly to git (no shell interpretation).
Optional SENTINEL is called with (process event) on completion.
Binds `default-directory' to `gnosis-dir' so sentinels run in
the correct directory regardless of buffer context."
  (let* ((default-directory gnosis-dir)
         (git (or (executable-find "git")
                  (error "Git is not installed or not in PATH")))
         (process (apply #'start-process "gnosis-git" nil git args)))
    (set-process-filter
     process
     (lambda (proc output)
       (when (string-match-p "password:" output)
         (process-send-string proc
			      (concat (read-passwd "Password: ") "\n")))
       (message "%s" output)))
    (when sentinel
      (set-process-sentinel process sentinel))
    process))

(defun gnosis--ensure-git-repo ()
  "Ensure `gnosis-dir' is a git repository."
  (let ((default-directory gnosis-dir))
    (unless (file-exists-p (expand-file-name ".git" gnosis-dir))
      (vc-git-create-repo))))

(defun gnosis--git-chain (commands &optional on-finish)
  "Run git COMMANDS sequentially, each as an arg list for `gnosis--git-cmd'.
Call ON-FINISH with no args after the last command succeeds.
Abort chain on failure with a message."
  (if (null commands)
      (when on-finish (funcall on-finish))
    (gnosis--git-cmd (car commands)
      (lambda (_proc event)
        (if (string-match-p "finished" event)
            (gnosis--git-chain (cdr commands) on-finish)
          (message "gnosis: git %s failed: %s"
                   (car (car commands)) (string-trim event)))))))

;;;###autoload
(defun gnosis-vc-push ()
  "Run `git push' for gnosis repository."
  (interactive)
  (gnosis--git-cmd '("push")))

;;;###autoload
(defun gnosis-vc-pull ()
  "Run `git pull' for gnosis repository.

Reopens the gnosis database after successful pull."
  (interactive)
  (gnosis--git-cmd
   '("pull")
   (lambda (proc event)
     (cond
      ((string-match-p "finished" event)
       (when (zerop (process-exit-status proc))
	 (condition-case err
	     (progn
	       (when (and gnosis-db (gnosis-sqlite-live-p gnosis-db))
		 (gnosis-sqlite-close gnosis-db))
	       (setf gnosis-db
                     (gnosis-sqlite-open (expand-file-name "gnosis.db" gnosis-dir)))
	       (gnosis-db-init)
	       (message "Gnosis: Pull successful, database reopened"))
	   (error (message "Gnosis: Failed to reopen database: %s"
			   (error-message-string err))))))
      ((string-match-p "exited abnormally" event)
       (message "Gnosis: Git pull failed with exit code %s"
                (process-exit-status proc)))))))

;; Gnosis mode ;;
;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode gnosis-modeline-mode
  "Minor mode for showing gnosis total due themata on modeline."
  :global t
  :group 'gnosis
  :lighter nil
  (setq gnosis-due-themata-total (length (gnosis-review-get-due-themata)))
  (if gnosis-modeline-mode
      (progn
        (add-to-list 'global-mode-string
                     '(:eval
                       (if (and gnosis-due-themata-total (> gnosis-due-themata-total 0))
                           (propertize (format " [%d] " gnosis-due-themata-total) 'face 'warning
                                       'gnosis-modeline t)
                         "")))
        (force-mode-line-update))
    (setq global-mode-string
          (seq-remove (lambda (item)
                        (and (listp item)
                             (eq (car item) :eval)
                             (get-text-property 0 'gnosis-modeline (format "%s" (eval (cadr item))))))
                      global-mode-string))
    (force-mode-line-update)))

(define-derived-mode gnosis-mode special-mode "Gnosis"
  "Gnosis Mode."
  :interactive nil
  (read-only-mode 0)
  (display-line-numbers-mode 0)
  ;; Initialize centering based on user preference
  (setq-local gnosis-center-content gnosis-center-content-during-review)
  :lighter " gnosis-mode")

;;; Bulk link operations

(defun gnosis--themata-to-update (themata string node-id)
  "Return list of (ID . NEW-KEIMENON) for THEMATA needing updates."
  (cl-loop for thema in themata
           for thema-id = (nth 0 thema)
           for keimenon = (nth 1 thema)
           for result = (gnosis-utils-replace-string-with-link keimenon string node-id)
           when (car result)
           collect (cons thema-id (cdr result))))

(defun gnosis--update-themata-keimenon (updates)
  "Apply UPDATES list of (ID . NEW-KEIMENON) to database."
  (gnosis-sqlite-with-transaction (gnosis--ensure-db)
    (dolist (update updates)
      (gnosis-update 'themata `(= keimenon ,(cdr update)) `(= id ,(car update))))))

(defun gnosis--commit-bulk-link (count string)
  "Commit bulk link changes for COUNT themata with STRING."
  (unless gnosis-testing
    (gnosis--ensure-git-repo)
    (gnosis--git-chain
     `(("add" "gnosis.db")
       ("commit" "-m"
        ,(format "Bulk link: %d themata updated with %s" count string)))
     (lambda ()
       (when gnosis-vc-auto-push (gnosis-vc-push))))))

(defun gnosis-bulk-link-themata (ids string node-id)
  "Replace STRING with org-link to NODE-ID in themata with IDS.
Return list of updated thema IDs."
  (when (string-empty-p string)
    (user-error "String cannot be empty"))
  (unless node-id
    (user-error "Node not found"))
  (let* ((themata (gnosis-select '[id keimenon] 'themata
                                 `(in id ,(vconcat ids))))
         (updates (gnosis--themata-to-update themata string node-id)))
    (if (null updates)
        (progn (message "No themata to update for '%s'" string) nil)
      (when (y-or-n-p (format "Replace '%s' in %d themata? " string (length updates)))
        (gnosis--update-themata-keimenon updates)
        (gnosis--commit-bulk-link (length updates) string)
        (message "Updated %d themata with links to '%s'" (length updates) string)
        (mapcar #'car updates)))))

(defun gnosis-bulk-link-string (string node-id)
  "Replace all instances of STRING in themata keimenon with org-link to NODE-ID."
  (interactive
   (let* ((string (read-string "String to replace: "))
          (nodes (gnosis-select '[id title] 'nodes))
          (node-title (gnosis-completing-read "Select node: " (mapcar #'cadr nodes)))
          (node-id (car (cl-find node-title nodes :key #'cadr :test #'string=))))
     (list string node-id)))
  (gnosis-bulk-link-themata (gnosis-collect-thema-ids :query string) string node-id))

;;; Link integrity

(defun gnosis--all-link-dests ()
  "Return all unique dest UUIDs from gnosis thema-links table."
  (cl-remove-duplicates (gnosis-select 'dest 'thema-links nil t) :test #'equal))

(defun gnosis--all-node-ids ()
  "Return all node IDs from both nodes and journal tables."
  (append (gnosis-select 'id 'nodes nil t)
          (gnosis-select 'id 'journal nil t)))

(defun gnosis--orphaned-link-dests ()
  "Return dest UUIDs in thema-links that have no matching node or journal entry."
  (let ((link-dests (gnosis--all-link-dests))
        (node-ids (gnosis--all-node-ids)))
    (cl-set-difference link-dests node-ids :test #'equal)))

(defun gnosis--orphaned-links ()
  "Return (source dest) rows where dest has no matching node."
  (let ((orphaned-dests (gnosis--orphaned-link-dests)))
    (when orphaned-dests
      (gnosis-select '[source dest] 'thema-links
                     `(in dest ,(vconcat orphaned-dests))))))

(defun gnosis--node-links-missing-dest ()
  "Return (source dest) pairs from node-links where dest has no matching node."
  (let* ((all-links (gnosis-select '[source dest] 'node-links nil))
         (node-ids (gnosis--all-node-ids))
         (id-set (make-hash-table :test 'equal)))
    (dolist (id node-ids)
      (puthash id t id-set))
    (cl-loop for (source dest) in all-links
             unless (gethash dest id-set)
             collect (list source dest))))

(defun gnosis--node-links-missing-source ()
  "Return (source dest) pairs from node-links where source has no matching node."
  (let* ((all-links (gnosis-select '[source dest] 'node-links nil))
         (node-ids (gnosis--all-node-ids))
         (id-set (make-hash-table :test 'equal)))
    (dolist (id node-ids)
      (puthash id t id-set))
    (cl-loop for (source dest) in all-links
             unless (gethash source id-set)
             collect (list source dest))))

(defun gnosis--delete-broken-node-links (broken-links)
  "Delete BROKEN-LINKS list of (source dest) from node-links table."
  (when broken-links
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (dolist (link broken-links)
        (gnosis-sqlite-execute (gnosis--ensure-db)
                               "DELETE FROM node_links WHERE source = ? AND dest = ?"
                               (list (car link) (cadr link)))))))

(defun gnosis--thema-expected-links (keimenon parathema)
  "Extract expected link IDs from KEIMENON and PARATHEMA text."
  (cl-remove-duplicates
   (append (gnosis-extract-id-links keimenon)
           (gnosis-extract-id-links parathema))
   :test #'equal))

(defun gnosis--stale-links ()
  "Return (source dest) pairs in DB but not in thema text.
Fetches all themata, extras, and thema-links in bulk queries."
  (let* ((themata (gnosis-select '[id keimenon] 'themata nil))
         (extras (gnosis-select '[id parathema] 'extras nil))
         (all-links (gnosis-select '[source dest] 'thema-links nil))
         (extras-map (make-hash-table :test 'equal)))
    ;; Build extras lookup
    (dolist (extra extras)
      (puthash (car extra) (cadr extra) extras-map))
    ;; Find links in DB that aren't in text
    (cl-loop for (source dest) in all-links
             for keimenon = (cadr (cl-find source themata :key #'car))
             for parathema = (gethash source extras-map "")
             for expected = (gnosis--thema-expected-links
                             (or keimenon "") (or parathema ""))
             unless (member dest expected)
             collect (list source dest))))

(defun gnosis--missing-links ()
  "Return (source dest) pairs in thema text but not in DB.
Fetches all themata, extras, and thema-links in bulk queries."
  (let* ((themata (gnosis-select '[id keimenon] 'themata nil))
         (extras (gnosis-select '[id parathema] 'extras nil))
         (all-links (gnosis-select '[source dest] 'thema-links nil))
         (extras-map (make-hash-table :test 'equal))
         (links-set (make-hash-table :test 'equal)))
    ;; Build extras lookup
    (dolist (extra extras)
      (puthash (car extra) (cadr extra) extras-map))
    ;; Build existing links set
    (dolist (link all-links)
      (puthash (format "%s-%s" (car link) (cadr link)) t links-set))
    ;; Find links in text that aren't in DB
    (cl-loop for (id keimenon) in themata
             for parathema = (gethash id extras-map "")
             for expected = (gnosis--thema-expected-links
                             (or keimenon "") (or parathema ""))
             append (cl-loop for dest in expected
                             for key = (format "%s-%s" id dest)
                             unless (gethash key links-set)
                             collect (list id dest)))))

(defun gnosis--links-check-format-count (n)
  "Format count N with face: green for 0, warning for >0."
  (propertize (number-to-string n)
              'face (if (zerop n) 'success 'warning)))

(defun gnosis--links-report-insert-heading (text)
  "Insert bold heading TEXT into current buffer."
  (insert (propertize text 'face 'bold) "\n"))

(defun gnosis--links-report-format-id (id)
  "Format ID with human-readable context.
String IDs get a node/journal title, integer IDs get a keimenon excerpt."
  (cond
   ((stringp id)
    (let ((title (gnosis--links-report-node-title id)))
      (if title
          (format "%s (%s)" id (truncate-string-to-width title 50 nil nil "..."))
        (format "%s (deleted)" id))))
   ((integerp id)
    (let ((ctx (gnosis--links-report-thema-context id)))
      (if ctx
          (format "%s (%s)" id ctx)
        (format "%s" id))))
   (t (format "%s" id))))

(defun gnosis--links-report-insert-row (source dest)
  "Insert a link report row for SOURCE and DEST with resolved context."
  (insert (format "  source: %s\n    dest: %s\n"
                  (gnosis--links-report-format-id source)
                  (gnosis--links-report-format-id dest))))

(defun gnosis--links-report-node-title (id)
  "Return the title for node ID, checking nodes then journal."
  (or (car (gnosis-select 'title 'nodes `(= id ,id) t))
      (car (gnosis-select 'title 'journal `(= id ,id) t))))

(defun gnosis--links-report-thema-context (thema-id)
  "Return a short keimenon excerpt for THEMA-ID."
  (let ((keimenon (car (gnosis-select 'keimenon 'themata `(= id ,thema-id) t))))
    (when (and keimenon (stringp keimenon) (> (length keimenon) 0))
      (truncate-string-to-width keimenon 60 nil nil "..."))))

(defun gnosis--links-report-generate (orphaned stale missing nl-missing-dest nl-missing-source)
  "Generate *Gnosis Link Report* buffer.
ORPHANED, STALE, MISSING are thema-links (source dest) lists.
NL-MISSING-DEST, NL-MISSING-SOURCE are node-links (source dest) lists."
  (with-current-buffer (get-buffer-create "*Gnosis Link Report*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (gnosis--links-report-insert-heading "Thema-links: orphaned dest")
      (if orphaned
          (dolist (row orphaned)
            (gnosis--links-report-insert-row (car row) (cadr row)))
        (insert "  None\n"))
      (insert "\n")
      (gnosis--links-report-insert-heading "Thema-links: stale (in DB but not in text)")
      (if stale
          (dolist (row stale)
            (gnosis--links-report-insert-row (car row) (cadr row)))
        (insert "  None\n"))
      (insert "\n")
      (gnosis--links-report-insert-heading "Thema-links: missing (in text but not in DB)")
      (if missing
          (dolist (row missing)
            (gnosis--links-report-insert-row (car row) (cadr row)))
        (insert "  None\n"))
      (insert "\n")
      (gnosis--links-report-insert-heading "Node-links: missing dest")
      (if nl-missing-dest
          (dolist (row nl-missing-dest)
            (gnosis--links-report-insert-row (car row) (cadr row)))
        (insert "  None\n"))
      (insert "\n")
      (gnosis--links-report-insert-heading "Node-links: missing source")
      (if nl-missing-source
          (dolist (row nl-missing-source)
            (gnosis--links-report-insert-row (car row) (cadr row)))
        (insert "  None\n"))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun gnosis-links-check ()
  "Report link health for thema-links and node-links."
  (interactive)
  (let ((orphaned-dests (gnosis--orphaned-link-dests))
        (stale (gnosis--stale-links))
        (missing (gnosis--missing-links))
        (nl-missing-dest (gnosis--node-links-missing-dest))
        (nl-missing-source (gnosis--node-links-missing-source))
        (orphaned-rows nil))
    (let ((has-issues (or orphaned-dests stale missing nl-missing-dest nl-missing-source))
          (summary (format "%s\n  thema-links: %s orphaned dest, %s stale, %s missing in DB\n  node-links: %s missing dest, %s missing source"
                           (propertize "Link health:" 'face 'bold)
                           (gnosis--links-check-format-count (length orphaned-dests))
                           (gnosis--links-check-format-count (length stale))
                           (gnosis--links-check-format-count (length missing))
                           (gnosis--links-check-format-count (length nl-missing-dest))
                           (gnosis--links-check-format-count (length nl-missing-source)))))
      (if (not has-issues)
          (message "%s" summary)
        (if (y-or-n-p "Issues found, view log? ")
            (progn
              (setq orphaned-rows (gnosis--orphaned-links))
              (gnosis--links-report-generate orphaned-rows stale missing
                                             nl-missing-dest nl-missing-source))
          (message "%s" summary))))))

(defun gnosis--delete-orphaned-links (orphaned-dests)
  "Delete thema-links whose dest is in ORPHANED-DESTS."
  (when orphaned-dests
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (gnosis-sqlite-execute-batch (gnosis--ensure-db)
        "DELETE FROM thema_links WHERE dest IN (%s)"
        orphaned-dests))))

(defun gnosis--delete-stale-links (stale-links)
  "Delete STALE-LINKS list of (source dest) from thema-links table."
  (when stale-links
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (dolist (link stale-links)
	(gnosis-sqlite-execute (gnosis--ensure-db)
			       "DELETE FROM thema_links WHERE source = ? AND dest = ?"
			       (list (car link) (cadr link)))))))

(defun gnosis--insert-missing-links (missing-links)
  "Insert MISSING-LINKS list of (source dest) into thema-links table."
  (when missing-links
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (dolist (link missing-links)
        (gnosis--insert-into 'thema-links `([,(car link) ,(cadr link)]))))))

(defun gnosis--commit-link-cleanup (orphaned stale missing &optional node-links-removed)
  "Commit link cleanup changes.
ORPHANED, STALE, MISSING are thema-links counts.
NODE-LINKS-REMOVED is the number of broken node-links deleted."
  (unless gnosis-testing
    (gnosis--ensure-git-repo)
    (gnosis--git-chain
     `(("add" "gnosis.db")
       ("commit" "-m"
        ,(format "Link cleanup: thema-links %d orphaned, %d stale, %d missing; node-links %d removed"
                 orphaned stale missing (or node-links-removed 0))))
     (lambda ()
       (when gnosis-vc-auto-push (gnosis-vc-push))))))

;;;###autoload
(defun gnosis-links-cleanup ()
  "Remove orphaned and stale thema-links and broken node-links."
  (interactive)
  (let ((orphaned-dests (gnosis--orphaned-link-dests))
        (stale (gnosis--stale-links))
        (nl-broken (append (gnosis--node-links-missing-dest)
                           (gnosis--node-links-missing-source))))
    (setq nl-broken (cl-remove-duplicates nl-broken :test #'equal))
    (if (and (null orphaned-dests) (null stale) (null nl-broken))
        (message "No broken links found")
      (when (y-or-n-p
             (format "Remove %d orphaned + %d stale thema-links, %d broken node-links? "
                     (length orphaned-dests) (length stale) (length nl-broken)))
        (gnosis--delete-orphaned-links orphaned-dests)
        (gnosis--delete-stale-links stale)
        (gnosis--delete-broken-node-links nl-broken)
        (gnosis--commit-link-cleanup (length orphaned-dests) (length stale) 0
                                     (length nl-broken))
        (message "Removed %d orphaned + %d stale thema-links, %d broken node-links"
                 (length orphaned-dests) (length stale) (length nl-broken))))))

;;;###autoload
(defun gnosis-links-sync ()
  "Full re-sync: remove orphaned/stale thema-links, broken node-links, and insert missing."
  (interactive)
  (let ((orphaned-dests (gnosis--orphaned-link-dests))
        (stale (gnosis--stale-links))
        (missing (gnosis--missing-links))
        (nl-broken (cl-remove-duplicates
                    (append (gnosis--node-links-missing-dest)
                            (gnosis--node-links-missing-source))
                    :test #'equal)))
    (if (and (null orphaned-dests) (null stale) (null missing) (null nl-broken))
        (message "All links are in sync")
      (when (y-or-n-p
             (format "Sync: remove %d orphaned + %d stale thema-links, %d broken node-links, add %d missing? "
                     (length orphaned-dests) (length stale) (length nl-broken) (length missing)))
        (gnosis--delete-orphaned-links orphaned-dests)
        (gnosis--delete-stale-links stale)
        (gnosis--delete-broken-node-links nl-broken)
        (gnosis--insert-missing-links missing)
        (gnosis--commit-link-cleanup (length orphaned-dests) (length stale)
                                     (length missing) (length nl-broken))
        (message "Synced: removed %d orphaned + %d stale thema-links, %d broken node-links, added %d missing"
                 (length orphaned-dests) (length stale) (length nl-broken) (length missing))))))

(provide 'gnosis)
;;; gnosis.el ends here
