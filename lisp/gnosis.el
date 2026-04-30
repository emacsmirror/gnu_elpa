;;; gnosis.el --- Knowledge System  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://codeberg.org/thanosapollo/emacs-gnosis

;; Version: 0.10.4

;; Package-Requires: ((emacs "29.1") (compat "29.1.4.2") (keymap-popup "0.2.0"))

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

(require 'gnosis-db)
(require 'gnosis-vc)
(require 'gnosis-tags)
(require 'gnosis-custom-values)
(require 'gnosis-links)
(require 'animate)

(require 'org)
(require 'org-element)

(require 'gnosis-algorithm)
(require 'gnosis-monkeytype)
(require 'gnosis-utils)
(require 'gnosis-org)
(require 'gnosis-cloze)
(require 'gnosis-nodes)
(require 'gnosis-journal)

(defgroup gnosis nil
  "Knowledege Management System For Note Taking & Self Testing."
  :group 'external
  :prefix "gnosis-")

(defcustom gnosis-string-difference 1
  "Threshold value for string comparison in Gnosis.

This variable determines the maximum acceptable Levenshtein distance
between two strings to consider them as similar."
  :type 'integer)

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

(autoload 'gnosis-dashboard "gnosis-dashboard" nil t)

(defvar gnosis-thema-types
  '(("Basic" . gnosis-add-thema--basic)
    ("MCQ" .  gnosis-add-thema--mcq)
    ("Double" .  gnosis-add-thema--double)
    ("Cloze" . gnosis-add-thema--cloze)
    ("MC-cloze" . gnosis-add-thema--mc-cloze))
  "Mapping of Themata & their respective functions.")

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


(defvar gnosis-review-editing-p nil
  "Boolean value to check if user is currently in a review edit.")

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

(defun gnosis-mcq-answer (id)
  "Choose the correct answer, from mcq choices for question ID."
  (let ((choices (gnosis-get 'hypothesis 'themata `(= id ,id)))
	(history-add-new-input nil)) ;; Disable history
    (gnosis-completing-read "Answer: " choices)))

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

(defun gnosis-suspended-p (id)
  "Return t if thema with ID is suspended."
  (= (gnosis-get 'suspend 'review-log `(= id ,id)) 1))

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

(provide 'gnosis)
;;; gnosis.el ends here
