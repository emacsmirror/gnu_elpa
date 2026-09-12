;;; gnosis-dashboard.el --- Dashboard for Gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis

;; Version: 0.0.1


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

;;; Code:

(require 'gnosis)
(require 'gnosis-db)
(require 'gnosis-logical-day)
(require 'gnosis-tags)
(require 'gnosis-links)
(require 'gnosis-tl)
(require 'gnosis-nodes)
(require 'keymap-popup)

(declare-function gnosis-review-resume "gnosis-review" ())

(defface gnosis-face-dashboard-header
  '((t :inherit (bold font-lock-constant-face)))
  "Face for dashboard header.

Avoid using an increased height value as this messes up with
`gnosis-center-string' implementation"
  :group 'gnosis)

(defcustom gnosis-dashboard-nodes-default-sort-column "Backlinks"
  "Default column to sort nodes dashboard by."
  :type '(radio (const :tag "Title" "Title")
                (const :tag "Links (forward links count)" "Links")
                (const :tag "Backlinks (backlinks count)" "Backlinks")
                (const :tag "Themata (themata links count)" "Themata"))
  :group 'gnosis)

(defcustom gnosis-dashboard-nodes-default-sort-ascending nil
  "Whether to sort nodes dashboard in ascending order.

When nil, sort in descending order (larger values first).
When non-nil, sort in ascending order (smaller values first)."
  :type 'boolean
  :group 'gnosis)

(defvar gnosis-dashboard-timer-delay 0.01)

(defvar gnosis-dashboard-buffer-name "*Gnosis Dashboard*"
  "Name of gnosis-dashboard buffer.")

(defvar-local gnosis-dashboard--current
  '(:type nil)
  "Current dashboard view to return to after edits.")

(defvar-local gnosis-dashboard--selected-ids nil
  "Selected IDs in this dashboard buffer.")


(defvar gnosis-dashboard-modules
  '(gnosis-dashboard-module-header
    gnosis-dashboard-module-today-stats
    gnosis-dashboard-module-average-rev))

(defvar-local gnosis-dashboard--history nil
  "Stack of previous views owned by this dashboard buffer.")
(put 'gnosis-dashboard--history 'permanent-local t)

(defvar-local gnosis-dashboard-themata-current-ids nil
  "Current thema IDs, including rows awaiting progressive rendering.")

(defvar-local gnosis-dashboard-nodes-current-ids nil
  "Current list of node IDs being displayed.")

(defvar-local gnosis-dashboard-tags-current nil
  "Current list of tags being displayed.")

(defvar-local gnosis-dashboard--link-issues nil
  "Count of link issues, computed on dashboard load.")

(defvar-local gnosis-dashboard--load-generation 0
  "Generation counter to cancel stale work in this buffer.")
(put 'gnosis-dashboard--load-generation 'permanent-local t)

(defvar-local gnosis-dashboard--timer nil
  "Timer owned by the current dashboard view.")

(defvar-local gnosis-dashboard--pending-entries nil
  "Unrendered suffix of this view's collection.
The rendered prefix is `tabulated-list-entries'.  Callbacks never retain
mutable tails of that prefix.  Mutations settle this suffix first.")

(defvar-local gnosis-dashboard--database nil
  "Database connection that produced the current view.")

(defvar gnosis-dashboard-module-header
  (lambda ()
    (insert "\n"
	    (gnosis-center-string
	     (format "%s" (propertize "Gnosis Dashboard" 'face
				      'gnosis-face-dashboard-header))))))

(defvar gnosis-dashboard-module-today-stats
  (lambda ()
    (let* ((due-count (length (gnosis-review-get--due-themata)))
           (overdue-count (gnosis-review-count-overdue)))
      (insert
       (gnosis-center-string
        (format "\nReviewed today: %s (New: %s)"
                (propertize
                 (number-to-string (gnosis-get-date-total-themata))
                 'face 'success)
                (propertize
                 (number-to-string (gnosis-get-date-new-themata))
                 'face 'font-lock-keyword-face)))
       "\n"
       (gnosis-center-string
        (format "Due themata: %s (Overdue: %s)"
                (propertize (number-to-string due-count) 'face 'error)
                (propertize (number-to-string overdue-count)
                            'face 'warning)))))))

(defvar gnosis-dashboard-module-average-rev
  (lambda ()
    (insert
     (gnosis-center-string
      (format "Reviews per active day: %s"
	      (propertize
	       (format "%.2f" (gnosis-calculate-average-daily-reviews))
	       'face 'font-lock-type-face)))
     "\n"
     (gnosis-center-string
      (format "Current streak: %s day(s)"
	      (propertize
	       (gnosis-dashboard--streak
		(cl-loop for (date total) in (gnosis-review-activity)
			 when (> total 0) collect date))
	       'face 'success))))))


(defun gnosis-dashboard--push-current-view (&optional view)
  "Push VIEW or the current list view onto navigation history."
  (let* ((snapshot (or view
                       (pcase major-mode
                         ('gnosis-dashboard-themata-mode
                          (cons 'themata gnosis-dashboard-themata-current-ids))
                         ('gnosis-dashboard-nodes-mode
                          (cons 'nodes gnosis-dashboard-nodes-current-ids))
                         ('gnosis-dashboard-tags-mode
                          (cons 'tags gnosis-dashboard-tags-current)))))
         (items (cdr snapshot)))
    (when (or view items)
      (push (list :type (car snapshot)
                  :id (when (derived-mode-p 'tabulated-list-mode)
                        (tabulated-list-get-id))
                  :items (copy-sequence items))
            gnosis-dashboard--history))))

(defun gnosis-dashboard--goto-id (id)
  "Move point to dashboard row ID when it is present."
  (when id
    (goto-char (point-min))
    (while (and (not (eobp))
                (not (equal (tabulated-list-get-id) id)))
      (forward-line 1))
    (equal (tabulated-list-get-id) id)))

(defun gnosis-dashboard--restore-view (view)
  "Restore dashboard VIEW from navigation history."
  (let ((id (plist-get view :id))
        (items (plist-get view :items)))
    (pcase-exhaustive (plist-get view :type)
      ('themata (gnosis-dashboard-output-themata items id))
      ('nodes
       (gnosis-dashboard-output-nodes items)
       (gnosis-dashboard--goto-id id))
      ('tags
       (gnosis-dashboard-output-tags items)
       (gnosis-dashboard--goto-id id)))))

(defun gnosis-dashboard--back ()
  "Restore the previous dashboard view, or open the main dashboard."
  (if gnosis-dashboard--history
      (gnosis-dashboard--restore-view (pop gnosis-dashboard--history))
    (gnosis-dashboard)))

(defun gnosis-dashboard-return (&optional current-values)
  "Return to the current dashboard view.
When non-nil, CURRENT-VALUES is a plist whose `:type' selects the view."
  (interactive)
  (let ((current-values (or current-values gnosis-dashboard--current)))
    (pcase (plist-get current-values :type)
      ('themata
       (gnosis-dashboard-output-themata
        gnosis-dashboard-themata-current-ids))
      ('tags
       (gnosis-dashboard-output-tags))
      ('history
       (gnosis-dashboard-history)))))

(defun gnosis-dashboard--streak (dates)
  "Return current review streak number as a string.
DATES: Dates in the activity log, a list of YYYYMMDD integers."
  (let ((date-set (make-hash-table :test 'eql))
        (count 0))
    (dolist (d dates)
      (puthash d t date-set))
    (cl-loop for i from -1 downto -9999
             for d1 = (gnosis-date i)
             while (gethash (gnosis--date-to-int d1) date-set)
             do (cl-incf count))
    (when (gethash (gnosis--today-int) date-set)
      (cl-incf count))
    (number-to-string count)))

(defun gnosis-dashboard-edit-thema ()
  "Edit thema with ID."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (gnosis-edit-thema id)))

(defun gnosis-dashboard-suspend-thema ()
  "Suspend or unsuspend themata.
With \\[universal-argument], unsuspend.  Without it, suspend.
For a single thema (no selection), toggles current value."
  (interactive nil gnosis-dashboard-themata-mode)
  (let* ((ids (or gnosis-dashboard--selected-ids
                  (list (tabulated-list-get-id))))
         (suspend-value (when (> (length ids) 1)
                          (if current-prefix-arg 0 1))))
    (gnosis-toggle-suspend-themata ids suspend-value)
    (gnosis-dashboard--update-entries ids)
    (setq gnosis-dashboard--selected-ids nil)
    (when (and (not current-prefix-arg) (> (length ids) 1))
      (message
       (format "Use %s to unsuspend."
               (propertize "C-u s"
                           'face 'font-lock-constant-face))))))

(defun gnosis-dashboard-delete ()
  "Delete marked themata or thema at point."
  (interactive nil gnosis-dashboard-themata-mode)
  (let ((ids (or gnosis-dashboard--selected-ids
                 (list (tabulated-list-get-id)))))
    (when (y-or-n-p (format "Delete %d themata?" (length ids)))
      (gnosis-delete-themata ids)
      (gnosis-dashboard--remove-entries ids)
      (setq gnosis-dashboard--selected-ids nil))))

(defun gnosis-dashboard-search-thema (&optional str)
  "Search for themata with STR."
  (interactive)
  ;; Save current themata view and position to history
  ;; before showing new search results
  (when gnosis-dashboard-themata-current-ids
    (gnosis-dashboard--push-current-view))
  (gnosis-dashboard-output-themata
   (gnosis-collect-thema-ids
    :query (or str (read-string "Search for thema: ")))))

(defun gnosis-dashboard-filter-themata (&optional str ids)
  "Filter themata IDS by searching within them for STR.
If IDS is not provided, use current themata being displayed."
  (interactive)
  (let* ((ids (or ids gnosis-dashboard-themata-current-ids))
         (query (or str (read-string "Filter current themata: "))))
    ;; Validate inputs
    (unless ids (user-error "No themata to filter"))
    (when (string-empty-p query) (user-error "Search query cannot be empty"))
    ;; Filter and display
    (let ((filtered (cl-intersection ids
                                     (gnosis-collect-thema-ids :query query)
                                     :test #'equal)))
      (if filtered
          (progn
            ;; Save current position and IDs to history
            (gnosis-dashboard--push-current-view)
            (gnosis-dashboard-output-themata filtered))
        (message "No themata match the filter")))))

(defun gnosis-dashboard-themata-show-new (max-reviews)
  "Show themata with at most MAX-REVIEWS total reviews.
With prefix arg, prompt for count.  Default 0 (never reviewed)."
  (interactive (list (if current-prefix-arg
                         (read-number "Max reviews: " 0)
                       0)))
  (let ((ids (gnosis-get-themata-by-reviews max-reviews)))
    (if ids
        (progn
          (setq gnosis-dashboard--history nil)
          (gnosis-dashboard-output-themata ids))
      (message "No themata with at most %d reviews" max-reviews))))

(defun gnosis-dashboard-filter-themata-by-reviews (max-reviews)
  "Filter current themata to those with at most MAX-REVIEWS total reviews.
With prefix arg, prompt for count.  Default 0 (never reviewed)."
  (interactive (list (if current-prefix-arg
                         (read-number "Max reviews: " 0)
                       0)))
  (unless gnosis-dashboard-themata-current-ids
    (user-error "No themata to filter"))
  (let ((filtered (gnosis-get-themata-by-reviews
                   max-reviews gnosis-dashboard-themata-current-ids)))
    (if filtered
        (progn
          (gnosis-dashboard--push-current-view)
          (gnosis-dashboard-output-themata filtered))
      (message "No themata in current view with at most %d reviews"
               max-reviews))))

(defun gnosis-dashboard-themata-back ()
  "Go back to the previous themata view, nodes view, or main dashboard."
  (interactive)
  (gnosis-dashboard--back))

(keymap-popup-define gnosis-dashboard-common-map
  :parent tabulated-list-mode-map
  :group "Common"
  "g" ("Refresh" gnosis-dashboard-return :stay-open t)
  :group "Mark"
  "m" ("Toggle mark" gnosis-dashboard-mark-toggle :stay-open t)
  "M" ("Mark all" gnosis-dashboard-mark-all :stay-open t)
  "u" ("Unmark" gnosis-dashboard-mark-toggle :stay-open t)
  "U" ("Unmark all" gnosis-dashboard-unmark-all :stay-open t))

(keymap-popup-define gnosis-dashboard-themata-mode-map
  "Themata"
  :parent gnosis-dashboard-common-map
  :description (lambda ()
                 (format "Themata (%s)"
                         (propertize
                          (number-to-string
                           (length gnosis-dashboard-themata-current-ids))
                          'face 'font-lock-type-face)))
  :group "Navigate"
  "q" ("Back" gnosis-dashboard-themata-back)
  "SPC" ("Search" gnosis-dashboard-search-thema)
  "l" ("Filter current" gnosis-dashboard-filter-themata)
  "n" ("Filter new/low reviews" gnosis-dashboard-filter-themata-by-reviews)
  "RET" ("Edit at point" gnosis-dashboard-edit-thema)
  :group "Edit"
  "e" ("Edit thema" gnosis-dashboard-edit-thema :stay-open t)
  "a" ("Add thema" gnosis-add-thema :stay-open t)
  "s" ("Suspend" gnosis-dashboard-suspend-thema :stay-open t)
  "d" ("Delete" gnosis-dashboard-delete :stay-open t)
  "b" ("Bulk link" gnosis-dashboard-bulk-link :stay-open t)
  "t" ("Modify tags" gnosis-dashboard-modify-tags :stay-open t))

(define-derived-mode gnosis-dashboard-themata-mode
  tabulated-list-mode "Gnosis Themata"
  "Major mode for gnosis dashboard themata output."
  :keymap gnosis-dashboard-themata-mode-map
  :interactive nil
  (gnosis-dashboard--common-setup))

(defun gnosis-dashboard--format-entry (row)
  "Format database ROW as a tabulated-list entry with single-line cells.
ROW is (id keimenon hypothesis answer tags type suspend)."
  (let* ((fields (cl-loop for item in (cdr row)
                         for formatted =
                         (replace-regexp-in-string
                          "\n" " "
                          (if (listp item)
                              (mapconcat (lambda (x) (format "%s" x)) item ",")
                            (format "%s" item)))
                         collect (if (listp item)
                                     formatted
                                   (replace-regexp-in-string
                                    "\\[\\[id:[^]]+\\]\\[\\(.*?\\)\\]\\]"
                                    "\\1" formatted)))))
    (list (car row)
	  (vconcat (append (butlast fields)
			   (list (if (equal (car (last fields)) "1")
				     "Yes" "No")))))))

(defun gnosis-dashboard--output-themata (thema-ids)
  "Read current rows for THEMA-IDS in their requested order.
Formatting is independent of the database and the destination buffer.
No rows survive refresh in a separate cache: core and study writes are
therefore visible without dashboard-specific invalidation hooks."
  (cl-assert (listp thema-ids))
  (let ((entries (make-hash-table :test 'equal)))
    (dolist (row (gnosis-sqlite-select-batch
                 (gnosis--ensure-db)
                 "SELECT themata.id, themata.keimenon, themata.hypothesis, themata.answer, (SELECT '(' || GROUP_CONCAT(tag, ' ') || ')' FROM thema_tag WHERE thema_id = themata.id) AS tags, themata.type, scheduler_state.suspended FROM themata JOIN scheduler_state ON themata.id = scheduler_state.thema_id WHERE themata.id IN (%s)"
                 thema-ids))
      (puthash (car row) (gnosis-dashboard--format-entry row) entries))
    (cl-loop for id in thema-ids
             for entry = (gethash id entries)
             when entry collect entry)))

(defun gnosis-dashboard--update-entries (ids)
  "Re-fetch and update tabulated-list entries for IDS.
Settle pending rendering and re-sort when a sort key is active."
  (gnosis-dashboard--finish-render)
  (let* ((new-entries (gnosis-dashboard--output-themata ids))
         (update-map (make-hash-table :test 'equal)))
    (dolist (entry new-entries)
      (puthash (car entry) entry update-map))
    (setq tabulated-list-entries
          (mapcar (lambda (entry)
                    (or (gethash (car entry) update-map) entry))
                  tabulated-list-entries))
    (if tabulated-list-sort-key
        (gnosis-tl-print t)
      (dolist (entry new-entries)
        (gnosis-tl-replace-entry (car entry) (cadr entry))))))

(defun gnosis-dashboard--remove-entries (ids)
  "Remove IDS from the collection and its displayed rows.
Settle pending rendering before deleting the affected lines."
  (gnosis-dashboard--finish-render)
  (let ((id-set (make-hash-table :test 'equal)))
    (dolist (id ids) (puthash id t id-set))
    (setq tabulated-list-entries
          (cl-remove-if (lambda (entry) (gethash (car entry) id-set))
                        tabulated-list-entries)
          gnosis-dashboard-themata-current-ids
          (cl-remove-if (lambda (id) (gethash id id-set))
                        gnosis-dashboard-themata-current-ids))
    (dolist (id ids) (gnosis-tl-delete-entry id))))

(defun gnosis-dashboard-update-entry (id)
  "Update thema ID in the current themata view after `gnosis-save-hook'.
Other views read fresh data when next opened."
  (when (eq major-mode 'gnosis-dashboard-themata-mode)
    (gnosis-dashboard--update-entries (list id))))

(add-hook 'gnosis-save-hook #'gnosis-dashboard-update-entry)

(defcustom gnosis-dashboard-render-chunk-size 5000
  "Number of entries per chunk for progressive rendering.
The first chunk is rendered immediately; remaining chunks are
appended via timers so the UI stays responsive."
  :type '(integer :tag "Entries (at least 1)")
  :group 'gnosis)

(defvar gnosis-dashboard-chunk-size 500
  "Obsolete cache-warming chunk size; rows are now read on demand.")
(make-obsolete-variable 'gnosis-dashboard-chunk-size
                        "Cache warming is no longer needed" "0.10.6")

(defun gnosis-dashboard-warm-cache ()
  "Do nothing; dashboard rows are now read on demand."
  (declare (obsolete "Rows are read on demand; no warming is needed" "0.10.6"))
  nil)

(define-obsolete-function-alias 'gnosis-dashboard-rebuild-cache
  #'gnosis-dashboard-return "0.10.6")

(defun gnosis-dashboard--compute-column-format (width)
  "Compute the themata column format vector for window WIDTH.
Distributes available width (minus padding and column gaps)
proportionally so all columns fit."
  (let ((avail (- width 7)))
    `[("Keimenon"   ,(max 10 (/ (* avail 28) 100)) t)
      ("Hypothesis" ,(max 8  (/ (* avail 16) 100)) t)
      ("Answer"     ,(max 8  (/ (* avail 16) 100)) t)
      ("Tags"       ,(max 8  (/ (* avail 18) 100)) t)
      ("Type"       ,(max 5  (/ (* avail 10) 100)) t)
      ("Suspend"    ,(max 3  (/ (* avail 8) 100)) t)]))

(defun gnosis-dashboard--set-column-format ()
  "Set `tabulated-list-format' based on current window width."
  (setf tabulated-list-format
        (gnosis-dashboard--compute-column-format (window-width))))


(defun gnosis-dashboard--cancel-load ()
  "Retire this view's timer and any unrendered entries."
  (when (timerp gnosis-dashboard--timer)
    (cancel-timer gnosis-dashboard--timer))
  (setq gnosis-dashboard--timer nil
        gnosis-dashboard--pending-entries nil)
  (cl-incf gnosis-dashboard--load-generation))

(defun gnosis-dashboard--finish-render ()
  "Settle the collection before a synchronous mutation or sort.
Honor the active sort key and retire queued callbacks even if already fired."
  (let ((pending gnosis-dashboard--pending-entries))
    (gnosis-dashboard--cancel-load)
    (when pending
      ;; Sorting must own both spines, not a retained prefix or borrowed suffix.
      (setq tabulated-list-entries (append tabulated-list-entries pending nil))
      (if tabulated-list-sort-key
          (gnosis-tl-print t)
        (gnosis-tl-append-entries pending)))))

(defun gnosis-dashboard--progressive-render (entries gen &optional restore-id)
  "Render ENTRIES in bounded chunks for generation GEN.
When RESTORE-ID is non-nil, select it once rendered unless the user moves.
Appending or settling chunks preserves inputs and pending snapshots.
`tabulated-list-entries' is mutable Emacs-owned view state; copy it for
snapshots that must survive native table commands."
  (let* ((size (max 1 gnosis-dashboard-render-chunk-size))
         (first-chunk (seq-take entries size)))
    (setq tabulated-list-entries first-chunk
          gnosis-dashboard--pending-entries (nthcdr size entries))
    (gnosis-tl-print)
    (when (and restore-id (assoc restore-id first-chunk))
      (gnosis-dashboard--goto-id restore-id)
      (setq restore-id nil))
    (when gnosis-dashboard--pending-entries
      (setq gnosis-dashboard--timer
            (run-with-timer gnosis-dashboard-timer-delay nil
                            #'gnosis-dashboard--append-chunk
                            (current-buffer) gen restore-id (point))))))

(defun gnosis-dashboard--append-chunk (buffer gen &optional restore-id expected-point)
  "Append the next owned chunk to BUFFER for generation GEN.
Restore RESTORE-ID only if point still equals EXPECTED-POINT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (eq major-mode 'gnosis-dashboard-themata-mode)
                 (= gen gnosis-dashboard--load-generation)
                 (eq gnosis-dashboard--database gnosis-db)
                 gnosis-dashboard--pending-entries)
        (unless (eql (point) expected-point) (setq restore-id nil))
        (setq gnosis-dashboard--timer nil)
        ;; Native header sorting may have reordered the rendered prefix.
        ;; Settle and sort the collection rather than append out of order.
        (if tabulated-list-sort-key
            (gnosis-dashboard--finish-render)
          (let* ((size (max 1 gnosis-dashboard-render-chunk-size))
                 (chunk (seq-take gnosis-dashboard--pending-entries size)))
            (gnosis-tl-append-entries chunk)
            (setq tabulated-list-entries (append tabulated-list-entries chunk)
                  gnosis-dashboard--pending-entries
                  (nthcdr size gnosis-dashboard--pending-entries))
            (when (and restore-id (assoc restore-id chunk))
              (gnosis-dashboard--goto-id restore-id)
              (setq restore-id nil))
            (when gnosis-dashboard--pending-entries
              (setq gnosis-dashboard--timer
                    (run-with-timer gnosis-dashboard-timer-delay nil
                                    #'gnosis-dashboard--append-chunk
                                    buffer gen restore-id (point))))))))))

(defun gnosis-dashboard-output-themata (thema-ids &optional restore-id)
  "Display THEMA-IDS in the gnosis dashboard.
When RESTORE-ID is non-nil, select that row once rendered."
  (cl-assert (listp thema-ids) t "`thema-ids' must be a list of thema ids.")
  (let ((entries (gnosis-dashboard--output-themata thema-ids)))
    (pop-to-buffer-same-window gnosis-dashboard-buffer-name)
    (gnosis-dashboard-themata-mode)
    (gnosis-dashboard--set-column-format)
    (tabulated-list-init-header)
    (setq gnosis-dashboard--current '(:type themata)
          gnosis-dashboard-themata-current-ids (mapcar #'car entries)
          gnosis-dashboard--database gnosis-db)
    (gnosis-dashboard--progressive-render
     entries gnosis-dashboard--load-generation restore-id)))

(defun gnosis-dashboard-output-tag (tag)
  "Output TAG name and total themata."
  (let ((themata (gnosis-get-tag-themata tag)))
    `(,tag ,(number-to-string (length themata)))))

(defun gnosis-dashboard-sort-total-themata (entry1 entry2)
  "Sort function for the total themata column, for ENTRY1 and ENTRY2."
  (let ((total1 (string-to-number (elt (cadr entry1) 1)))
        (total2 (string-to-number (elt (cadr entry2) 1))))
    (< total1 total2)))

(defun gnosis-dashboard-rename-tag ()
  "Rename TAG to NEW-TAG."
  (interactive)
  (let ((current-line (line-number-at-pos)))
    (gnosis-tag-rename (tabulated-list-get-id))
    (gnosis-dashboard-output-tags)
    (forward-line (- current-line 1))))

(defun gnosis-dashboard-bulk-rename-tags ()
  "Bulk-rename marked (or all displayed) tags via regex.
Prompts for a regex pattern and replacement string, previews
which tags will be renamed (and how many will merge), then
applies via `gnosis--tag-rename-batch'."
  (interactive)
  (let* ((tags (or (and gnosis-dashboard--selected-ids
			(prog1 gnosis-dashboard--selected-ids
			  (setq gnosis-dashboard--selected-ids nil)))
		   gnosis-dashboard-tags-current))
	 (pattern (gnosis-dashboard--pcre-to-emacs
		   (read-string "Rename pattern (regex): ")))
	 (replacement (replace-regexp-in-string
		       "-" "_"
		       (read-string "Replacement: ")))
	 (all-tags (mapcar #'car (gnosis-sqlite-select (gnosis--ensure-db)
						       "SELECT DISTINCT tag FROM thema_tag")))
	 (existing-ht (let ((ht (make-hash-table :test 'equal)))
			(dolist (t1 all-tags ht)
			  (puthash t1 t ht))))
	 pairs)
    (dolist (tag tags)
      (let ((new (replace-regexp-in-string pattern replacement tag)))
	(unless (string= tag new)
	  (push (cons tag new) pairs))))
    (unless pairs
      (user-error "No tags match the pattern"))
    (let ((merges (cl-count-if (lambda (p) (gethash (cdr p) existing-ht))
			       pairs)))
      (when (y-or-n-p (format "Rename %d tag(s)%s?"
			      (length pairs)
			      (if (> merges 0)
				  (format " (%d will merge into existing)" merges)
				"")))
	(gnosis--tag-rename-batch pairs)
	(remove-overlays nil nil 'gnosis-mark t)
	(gnosis-dashboard-output-tags)))))

(defun gnosis-dashboard-merge-case-duplicates ()
  "Merge tags that differ only by case.
For each group of case-variants, the most-used tag is kept as
canonical; ties are broken alphabetically.  The rest are renamed
to the canonical form via `gnosis--tag-rename-batch'."
  (interactive)
  (let* ((tag-counts (gnosis-sqlite-select (gnosis--ensure-db)
					   "SELECT tag, COUNT(*) FROM thema_tag GROUP BY tag"))
	 (groups (make-hash-table :test 'equal))
	 pairs)
    ;; Group tags by downcased form
    (dolist (row tag-counts)
      (let ((tag (car row))
	    (count (cadr row)))
	(push (cons tag count) (gethash (downcase tag) groups))))
    ;; Build rename pairs for groups with >1 variant
    (maphash (lambda (_key variants)
	       (when (> (length variants) 1)
		 ;; Sort: highest count first, then alphabetically for ties
		 (let* ((sorted (sort variants
				      (lambda (a b)
					(if (= (cdr a) (cdr b))
					    (string< (car a) (car b))
					  (> (cdr a) (cdr b))))))
			(canonical (caar sorted)))
		   (dolist (v (cdr sorted))
		     (push (cons (car v) canonical) pairs)))))
	     groups)
    (if (not pairs)
	(message "No case-duplicate tags found")
      (when (y-or-n-p (format "Merge %d tag(s) into %d canonical form(s)?"
			      (length pairs)
			      (length (seq-uniq (mapcar #'cdr pairs)))))
	(gnosis--tag-rename-batch pairs)
	(remove-overlays nil nil 'gnosis-mark t)
	(gnosis-dashboard-output-tags)))))

(defun gnosis-dashboard-delete-tag (&optional tag)
  "Delete TAG or marked tags from all themata."
  (interactive)
  (let ((tags (or (and gnosis-dashboard--selected-ids
                       (prog1 gnosis-dashboard--selected-ids
                         (setq gnosis-dashboard--selected-ids nil)))
                  (list (or tag (tabulated-list-get-id))))))
    (when (y-or-n-p (format "Delete %d tag(s)?" (length tags)))
      (gnosis-sqlite-execute-batch (gnosis--ensure-db)
				   "DELETE FROM thema_tag WHERE tag IN (%s)"
				   tags)
      (remove-overlays nil nil 'gnosis-mark t)
      (setq tabulated-list-entries
            (cl-remove-if (lambda (entry) (member (car entry) tags))
                          tabulated-list-entries))
      (dolist (tag tags)
        (gnosis-tl-delete-entry tag)))))

(defun gnosis-dashboard-suspend-tag (&optional tag)
  "Suspend themata of TAG or marked tags."
  (interactive)
  (let* ((tags (or (and gnosis-dashboard--selected-ids
                        (prog1 gnosis-dashboard--selected-ids
                          (setq gnosis-dashboard--selected-ids nil)))
                   (list (or tag (tabulated-list-get-id)))))
         (themata (mapcar #'car
			  (gnosis-sqlite-select-batch (gnosis--ensure-db)
						      "SELECT DISTINCT thema_id FROM thema_tag WHERE tag IN (%s)"
						      tags)))
         (suspend (if current-prefix-arg 0 1))
         (action (if (= suspend 0) "Unsuspend" "Suspend")))
    (when (y-or-n-p (format "%s %d themata across %d tag(s)?"
                            action (length themata) (length tags)))
      (gnosis-toggle-suspend-themata themata suspend t)
      (remove-overlays nil nil 'gnosis-mark t)
      (message "%sed %d themata" action (length themata)))))

(defun gnosis-dashboard-tag-view-themata (&optional tag)
  "View themata for TAG."
  (interactive)
  (let ((tag (or tag (tabulated-list-get-id))))
    (gnosis-dashboard--push-current-view)
    (gnosis-dashboard-output-themata (gnosis-get-tag-themata tag))))

(keymap-popup-define gnosis-dashboard-tags-mode-map
  "Tags"
  :parent gnosis-dashboard-common-map
  :description (lambda ()
                 (format "Tags (%s)"
                         (propertize
                          (number-to-string
                           (length gnosis-dashboard-tags-current))
                          'face 'font-lock-type-face)))
  :group "Navigate"
  "RET" ("View themata" gnosis-dashboard-tag-view-themata)
  "q" ("Back" gnosis-dashboard-tags-back)
  "SPC" ("Search" gnosis-dashboard-search-tags)
  "l" ("Filter current" gnosis-dashboard-filter-tags)
  :group "Edit"
  "r" ("Rename tag" gnosis-dashboard-rename-tag :stay-open t)
  "R" ("Bulk regex rename" gnosis-dashboard-bulk-rename-tags :stay-open t)
  "C" ("Merge case duplicates" gnosis-dashboard-merge-case-duplicates :stay-open t)
  "s" ("Suspend tag" gnosis-dashboard-suspend-tag :stay-open t)
  "d" ("Delete tag" gnosis-dashboard-delete-tag :stay-open t))

(define-derived-mode gnosis-dashboard-tags-mode
  tabulated-list-mode "Gnosis Tags"
  "Major mode for dashboard output of tags."
  :keymap gnosis-dashboard-tags-mode-map
  :interactive nil
  (gnosis-dashboard--common-setup))

(cl-defun gnosis-dashboard-output-tags (&optional (tags nil tags-supplied-p))
  "Format the Gnosis dashboard with TAGS.
When TAGS is omitted, use every current database tag."
  (interactive)
  (let* ((tag-counts (gnosis-sqlite-select (gnosis--ensure-db)
					   "SELECT tag, COUNT(*) FROM thema_tag GROUP BY tag"))
         (count-ht (let ((ht (make-hash-table
                              :test 'equal
                              :size (length tag-counts))))
                     (dolist (row tag-counts ht)
                       (puthash (car row) (cadr row) ht))))
         (tags (if tags-supplied-p tags (mapcar #'car tag-counts))))
    (pop-to-buffer-same-window gnosis-dashboard-buffer-name)
    (gnosis-dashboard-tags-mode)
    (setq gnosis-dashboard-tags-current tags)
    (setf gnosis-dashboard--current '(:type tags))
    (setq tabulated-list-format
          [("Name" 35 t)
           ("Total Themata" 10
            gnosis-dashboard-sort-total-themata)])
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (cl-loop for tag in tags
                   collect (list tag
                                 (vector tag (number-to-string
                                              (gethash tag count-ht 0))))))
    (tabulated-list-print t)))

(defun gnosis-dashboard--pcre-to-emacs (pattern)
  "Convert PCRE-style braces in PATTERN to Emacs regex syntax.
Translates {n}, {n,}, {n,m} to \\{n\\}, \\{n,\\}, \\{n,m\\}."
  (replace-regexp-in-string
   "{\\([0-9]+,?[0-9]*\\)}"
   "\\\\{\\1\\\\}"
   pattern))

(defun gnosis-dashboard-filter-tags (&optional pattern)
  "Filter current tags view by regex PATTERN."
  (interactive)
  (unless gnosis-dashboard-tags-current
    (user-error "No tags to filter"))
  (let* ((pattern (gnosis-dashboard--pcre-to-emacs
                   (or pattern (read-string "Filter tags (regex): "))))
         (filtered (cl-remove-if-not
                    (lambda (tag) (string-match-p pattern tag))
                    gnosis-dashboard-tags-current)))
    (if filtered
        (progn
          (gnosis-dashboard--push-current-view)
          (gnosis-dashboard-output-tags filtered))
      (message "No tags match pattern: %s" pattern))))

(defun gnosis-dashboard-search-tags (&optional pattern)
  "Search all tags by regex PATTERN."
  (interactive)
  (let* ((pattern (gnosis-dashboard--pcre-to-emacs
                   (or pattern (read-string "Search tags (regex): "))))
         (all-tags (mapcar #'car (gnosis-select 'tag 'thema-tag)))
         (all-tags (seq-uniq all-tags))
         (filtered (cl-remove-if-not
                    (lambda (tag) (string-match-p pattern tag))
                    all-tags)))
    (if filtered
        (progn
          (when gnosis-dashboard-tags-current
            (gnosis-dashboard--push-current-view))
          (gnosis-dashboard-output-tags filtered))
      (message "No tags match pattern: %s" pattern))))

(defun gnosis-dashboard-tags-back ()
  "Go back to the previous tags view, or to main dashboard."
  (interactive)
  (gnosis-dashboard--back))

(defun gnosis-dashboard-history (&optional history)
  "Display review HISTORY."
  (interactive)
  (let* ((history (or history
		      (gnosis-review-activity)))
	 (buffer (get-buffer-create "*Gnosis History*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer))
      (tabulated-list-mode)
      (setq tabulated-list-format
            `[("Date" ,(/ (window-width) 6) t)
              ("Total Reviews" ,(/ (window-width) 6)
               gnosis-dashboard-sort-total-themata)
              ("New" ,(/ (window-width) 6)
               gnosis-dashboard-sort-total-themata)])
      (make-local-variable 'tabulated-list-entries)
      ;; Sort for date
      (setq tabulated-list-sort-key (cons "Date" t))
      (setq tabulated-list-entries
            (cl-loop for entry in history
                     for date = (gnosis--int-to-date (car entry))
                     collect (list (car entry)
                                   (vector (propertize
					    (format "%04d/%02d/%02d"
						    (nth 0 date)
						    (nth 1 date)
						    (nth 2 date))
					    'face 'org-date)
                                           (number-to-string
                                            (cadr entry))
                                           (number-to-string
                                            (caddr entry))))))
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (setq gnosis-dashboard--current
	    '(:type history)))
    (pop-to-buffer buffer)))

(keymap-popup-define gnosis-dashboard-nodes-map
  "Nodes"
  :description "Nodes"
  :group "Nodes"
  "a" ("View all nodes" (lambda () (interactive)
			  (setq gnosis-dashboard--history nil)
			  (gnosis-dashboard-output-nodes)))
  "t" ("View nodes by tag" gnosis-dashboard-nodes-search-by-tag)
  "i" ("View isolated nodes" (lambda () (interactive)
			       (setq gnosis-dashboard--history nil)
			       (gnosis-dashboard-output-nodes)
			       (gnosis-dashboard-nodes-show-isolated))))

(keymap-popup-define gnosis-dashboard-themata-map
  "Themata"
  :description "Themata"
  :group "Themata"
  "a" ("View all themata" (lambda () (interactive)
			    (setq gnosis-dashboard--history nil)
			    (gnosis-dashboard-output-themata (gnosis-collect-thema-ids))))
  "SPC" ("Search themata" gnosis-dashboard-suffix-query)
  "t" ("View by tags" gnosis-dashboard-view-by-tags)
  "T" ("View all tags" gnosis-dashboard-output-tags)
  "n" ("View new" gnosis-dashboard-themata-show-new)
  "o" ("Show orphaned" gnosis-dashboard-themata-show-orphaned))

(keymap-popup-define gnosis-dashboard-import-export-map
  "Import/Export"
  :description "Import/Export"
  :group "Import/Export"
  "e" ("Export themata" gnosis-export-db)
  "i" ("Import themata" gnosis-import-db)
  "I" ("Import Anki" gnosis-import-anki))

(keymap-popup-define gnosis-dashboard-maintenance-map
  "Maintenance"
  :description "Maintenance"
  :group "Maintenance"
  "s" ("Sync nodes" gnosis-nodes-db-sync)
  "S" ("Rebuild nodes" (lambda () (interactive) (gnosis-nodes-db-sync t)))
  "l" ((lambda ()
         (let ((n gnosis-dashboard--link-issues))
           (if (null n)
               "Link health"
             (format "Link health %s"
                     (propertize
                      (format "[%d %s]" n
                              (if (= n 1) "issue" "issues"))
                      'face (if (zerop n) 'success 'warning))))))
       gnosis-links-check)
  "L" ("Link sync" gnosis-links-sync)
  "m" ("Monkeytype" gnosis-monkeytype-start))

(keymap-popup-define gnosis-dashboard-mode-map
  "Gnosis Dashboard"
  :description "Gnosis Dashboard"
  :group "Navigate"
  "n" ("Nodes" :keymap gnosis-dashboard-nodes-map)
  "t" ("Themata" :keymap gnosis-dashboard-themata-map)
  "H" ("History" gnosis-dashboard-history)
  :group "Actions"
  "r" ("Review" gnosis-review)
  "a" ("Add thema" gnosis-add-thema)
  "SPC" ("Search" gnosis-dashboard-search-thema)
  :group "Study"
  "s" ("Study topic" gnosis-study-topic)
  "p" ("Practise topics" gnosis-practice-topic)
  "w" ("Repair questions" gnosis-study-repair)
  "R" ("Resume batch" gnosis-review-resume)
  :group "More"
  "x" ("Import/Export" :keymap gnosis-dashboard-import-export-map)
  "!" ("Maintenance" :keymap gnosis-dashboard-maintenance-map))

(defun gnosis-dashboard--common-setup ()
  "Common buffer setup for all dashboard views."
  (gnosis-dashboard--cancel-load)
  (setq gnosis-dashboard--database gnosis-db)
  (add-hook 'change-major-mode-hook #'gnosis-dashboard--cancel-load nil t)
  (add-hook 'kill-buffer-hook #'gnosis-dashboard--cancel-load nil t)
  (when (fboundp 'keymap-popup-dismiss)
    (keymap-popup-dismiss))
  (setq-local header-line-format nil)
  ;; Character "…" can mess up column-width depending on the font used.
  (setq-local truncate-string-ellipsis "...")
  (setq-local gnosis-center-content t)
  (setq tabulated-list-padding 2
        tabulated-list-entries nil
	tabulated-list-sort-key nil
	gnosis-dashboard--selected-ids nil)
  (display-line-numbers-mode 0))

(define-derived-mode gnosis-dashboard-mode
  tabulated-list-mode "Gnosis Dashboard"
  "Major mode for displaying Gnosis dashboard."
  :keymap gnosis-dashboard-mode-map
  :interactive nil
  (gnosis-dashboard--common-setup))

(defun gnosis-dashboard-enable-mode ()
  "Enable `gnosis-dashboard-mode' if not already in a dashboard mode."
  (when (and (string= (buffer-name) gnosis-dashboard-buffer-name)
	     (not (derived-mode-p 'gnosis-dashboard-mode
				  'gnosis-dashboard-themata-mode
				  'gnosis-dashboard-tags-mode
				  'gnosis-dashboard-nodes-mode)))
    (gnosis-dashboard-mode)))

(defun gnosis-dashboard-mark-toggle ()
  "Toggle mark on the current item in the tabulated-list."
  (interactive)
  (let ((inhibit-read-only t)
        (entry (tabulated-list-get-entry))
	(id (tabulated-list-get-id)))
    (if entry
        (let ((beg (line-beginning-position))
              (end (line-end-position))
              (overlays (overlays-in (line-beginning-position)
                                     (line-end-position))))
          (if (cl-some (lambda (ov) (overlay-get ov 'gnosis-mark)) overlays)
              (progn
                (remove-overlays beg end 'gnosis-mark t)
		(setq gnosis-dashboard--selected-ids
		      (remove id gnosis-dashboard--selected-ids)))
            (let ((ov (make-overlay beg end)))
	      (unless (member id gnosis-dashboard--selected-ids)
		(setf gnosis-dashboard--selected-ids
		      (cons id gnosis-dashboard--selected-ids)))
              (overlay-put ov 'face 'highlight)
              (overlay-put ov 'gnosis-mark t)))
	  (forward-line))
      (message "No entry at point"))))

(defun gnosis-dashboard-unmark-all ()
  "Unmark all items in the tabulated-list."
  (interactive)
  (let ((inhibit-read-only t))
    (setq gnosis-dashboard--selected-ids nil)
    (remove-overlays nil nil 'gnosis-mark t)
    (message "All items unmarked")))

(defun gnosis-dashboard-mark-all ()
  "Mark all items in the tabulated-list buffer and collect their IDs."
  (interactive)
  (when (derived-mode-p 'tabulated-list-mode)
    (let ((inhibit-read-only t))
      ;; Clear existing marks
      (remove-overlays (point-min) (point-max) 'gnosis-mark t)
      ;; Apply overlay to the entire buffer at once
      (let ((ov (make-overlay (point-min) (point-max))))
        (overlay-put ov 'face 'highlight)
        (overlay-put ov 'gnosis-mark t))
      ;; Set selected IDs from all entries
      (setq gnosis-dashboard--selected-ids
            (mapcar #'car tabulated-list-entries))
      (message "Marked %d items" (length gnosis-dashboard--selected-ids)))))


(defun gnosis-dashboard-bulk-link ()
  "Bulk link string in marked or all displayed themata."
  (interactive nil gnosis-dashboard-themata-mode)
  (let* ((ids (or gnosis-dashboard--selected-ids
                  gnosis-dashboard-themata-current-ids))
         (_ (unless ids (user-error "No themata to link")))
         (string (read-string "String to replace: "))
         (nodes (gnosis-select '[id title] 'nodes))
         (node-title (gnosis-completing-read
                      "Select node: "
                      (mapcar #'cadr nodes)))
         (node-id (car (cl-find node-title nodes
                                :key #'cadr
                                :test #'string=)))
         (updated (gnosis-bulk-link-themata ids string node-id)))
    (when updated
      (gnosis-dashboard--update-entries updated)
      (setq gnosis-dashboard--selected-ids nil))))

(defun gnosis-dashboard-modify-tags ()
  "Add or remove tags on marked or displayed thema at point.
Uses +tag/-tag syntax: +foo adds tag foo, -bar removes tag bar."
  (interactive nil gnosis-dashboard-themata-mode)
  (let* ((ids (or gnosis-dashboard--selected-ids
                  (list (tabulated-list-get-id))))
         (_ (unless ids (user-error "No themata to modify")))
         (tags (gnosis-get-tags--unique))
         (candidates (cl-loop for tag in tags
                              nconc (list (concat "+" tag)
                                          (concat "-" tag))))
         (input (completing-read-multiple
                 "Modify tags (+add -remove): " candidates))
         (parsed (gnosis-tags--parse-filter input))
         (add-tags (car parsed))
         (remove-tags (cdr parsed)))
    (when (or add-tags remove-tags)
      (gnosis-modify-thema-tags ids add-tags remove-tags)
      (setq gnosis-dashboard--selected-ids nil)
      (gnosis-dashboard-return)
      (message "Modified tags on %d themata" (length ids)))))

(defun gnosis-dashboard-suffix-query (query)
  "Search for thema content for QUERY."
  (interactive "sSearch for thema content: ")
  (gnosis-dashboard-output-themata (gnosis-collect-thema-ids :query query)))

(defun gnosis-dashboard-themata-show-orphaned ()
  "Show themata with orphaned links (referencing deleted nodes)."
  (interactive nil gnosis-dashboard-themata-mode)
  (let* ((orphaned-rows (gnosis--orphaned-links))
         (thema-ids (when orphaned-rows
                      (cl-remove-duplicates (mapcar #'car orphaned-rows)))))
    (if thema-ids
        (progn
          (setq gnosis-dashboard--history nil)
          (gnosis-dashboard-output-themata thema-ids))
      (message "No themata with orphaned links"))))

(defun gnosis-dashboard-view-by-tags ()
  "Prompt for tags and display matching themata."
  (interactive)
  (let* ((filter (gnosis-tags-filter-prompt))
	 (ids (gnosis-filter-by-tags (car filter) (cdr filter))))
    (when ids
      (gnosis-dashboard--push-current-view
       (cons 'tags
             (mapcar #'car
                     (gnosis-sqlite-select
                      (gnosis--ensure-db)
                      "SELECT DISTINCT tag FROM thema_tag"))))
      (gnosis-dashboard-output-themata ids))))


(defun gnosis-dashboard--compute-link-issues ()
  "Compute and cache the total number of link issues."
  (setq gnosis-dashboard--link-issues
        (+ (length (gnosis--orphaned-link-dests))
           (length (gnosis--stale-links))
           (length (gnosis--missing-links))
           (length (gnosis--node-links-missing-dest))
           (length (gnosis--node-links-missing-source)))))

(defun gnosis-dashboard--load-stats (buffer marker generation)
  "Load dashboard statistics into BUFFER at MARKER position.
GENERATION prevents stale updates when the user navigates away."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (eq major-mode 'gnosis-dashboard-mode)
                 (= generation gnosis-dashboard--load-generation)
                 (eq gnosis-dashboard--database gnosis-db))
        (let ((inhibit-read-only t))
          (delete-region marker (point-max))
          (goto-char marker)
          (let ((modules (cdr gnosis-dashboard-modules)))
            (funcall (symbol-value (car modules)))
            (dolist (module (cdr modules))
              (gnosis-insert-separator)
              (funcall (symbol-value module))))
          (goto-char (point-min)))
        (setq gnosis-dashboard--timer
              (run-with-idle-timer
               0.5 nil
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when (and (= generation gnosis-dashboard--load-generation)
                                (eq gnosis-dashboard--database gnosis-db))
                       (setq gnosis-dashboard--timer nil)
                       (gnosis-dashboard--compute-link-issues)))))))))))

;;;###autoload
(defun gnosis-dashboard ()
  "Launch gnosis dashboard."
  (interactive)
  (let* ((buffer (get-buffer-create gnosis-dashboard-buffer-name))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (gnosis-dashboard-mode)
      (setq gnosis-dashboard--history nil
            gnosis-dashboard--database (gnosis--ensure-db))
      ;; Show header immediately
      (funcall (symbol-value (car gnosis-dashboard-modules)))
      (gnosis-insert-separator)
      (let ((stats-start (point-marker))
            (gen gnosis-dashboard--load-generation))
        (insert (gnosis-center-string "Loading statistics..."))
        (pop-to-buffer-same-window buffer)
        (goto-char (point-min))
        (gnosis-dashboard-enable-mode)
        ;; Defer expensive stats modules
        (setq gnosis-dashboard--timer
              (run-with-timer gnosis-dashboard-timer-delay nil
                              #'gnosis-dashboard--load-stats
                              buffer stats-start gen)))
      (keymap-popup gnosis-dashboard-mode-map))))

(defun gnosis-dashboard-sort-count (entry1 entry2)
  "Sort function for numeric count columns.
Compares ENTRY1 and ENTRY2 by converting string values to numbers."
  (let* ((col-name (car tabulated-list-sort-key))
         (col-index (tabulated-list--column-number col-name)))
    (< (string-to-number (aref (cadr entry1) col-index))
       (string-to-number (aref (cadr entry2) col-index)))))

(defun gnosis-dashboard-get-themata-links (node-id)
  "Return list of thema IDs that link to NODE-ID.
Queries the thema-links table where dest = NODE-ID."
  (gnosis-select 'source 'thema-links `(= dest ,node-id) t))

(defun gnosis-dashboard-get-backlink-ids (node-id)
  "Return list of node IDs that link to NODE-ID (backlinks)."
  (gnosis-select 'source 'node-links `(= dest ,node-id) t))

(defun gnosis-dashboard-get-forward-link-ids (node-id)
  "Return list of node IDs that NODE-ID links to (forward links)."
  (gnosis-select 'dest 'node-links `(= source ,node-id) t))

(defun gnosis-dashboard-nodes--data (&optional node-ids)
  "Get nodes data formatted for `tabulated-list-mode'.
If NODE-IDS is provided, only get data for those nodes.
Returns list of (ID [TITLE LINK-COUNT BACKLINK-COUNT THEMATA-LINKS-COUNT])."
  (let* ((nodes-data (gnosis-nodes-get-nodes-data node-ids))
	 (all-ids (mapcar #'car nodes-data))
	 ;; Bulk fetch forward links (1 query instead of N)
	 (fwd-raw (if all-ids
		      (gnosis-select '[source dest] 'node-links
				     `(in source ,(vconcat all-ids)))
		    (gnosis-select '[source dest] 'node-links)))
	 (fwd-hash (let ((h (make-hash-table :test 'equal)))
		     (dolist (link fwd-raw h)
		       (puthash (nth 0 link)
				(1+ (or (gethash (nth 0 link) h) 0)) h))))
	 ;; Bulk fetch themata links (1 query instead of N)
	 (themata-raw (if all-ids
			  (gnosis-select '[dest source] 'thema-links
					 `(in dest ,(vconcat all-ids)))
			(gnosis-select '[dest source] 'thema-links)))
	 (themata-hash (let ((h (make-hash-table :test 'equal)))
			 (dolist (link themata-raw h)
			   (puthash (nth 0 link)
				    (1+ (or (gethash (nth 0 link) h) 0)) h)))))
    (mapcar
     (lambda (node)
       (let* ((id (nth 0 node))
	      (title (nth 1 node))
	      (link-count (number-to-string (or (gethash id fwd-hash) 0)))
	      (backlink-count (number-to-string (nth 2 node)))
	      (themata-links-count
	       (number-to-string (or (gethash id themata-hash) 0))))
	 (list id (vector title link-count backlink-count themata-links-count))))
     nodes-data)))

(defun gnosis-dashboard-nodes--show-related
    (get-ids-fn no-results-msg &optional display-fn)
  "Show related items for the node at point.

GET-IDS-FN takes a node-id and returns related IDs.
NO-RESULTS-MSG is displayed when no related items are found.
DISPLAY-FN displays results, defaults to `gnosis-dashboard-output-nodes'."
  (let* ((node-id (tabulated-list-get-id))
         (related-ids (funcall get-ids-fn node-id))
         (display-fn (or display-fn #'gnosis-dashboard-output-nodes)))
    (if related-ids
        (progn
          (gnosis-dashboard--push-current-view)
          (funcall display-fn related-ids))
      (message "%s" no-results-msg))))

(defun gnosis-dashboard-nodes-show-links ()
  "Show forward links of the node at point."
  (interactive)
  (gnosis-dashboard-nodes--show-related
   #'gnosis-dashboard-get-forward-link-ids
   "No forward links found for this node"))

(defun gnosis-dashboard-nodes-show-backlinks ()
  "Show backlinks of the node at point."
  (interactive)
  (gnosis-dashboard-nodes--show-related
   #'gnosis-dashboard-get-backlink-ids
   "No backlinks found for this node"))

(defun gnosis-dashboard-nodes-show-themata-links ()
  "Show themata that link to the node at point."
  (interactive)
  (gnosis-dashboard-nodes--show-related
   #'gnosis-dashboard-get-themata-links
   "No themata link to this node"
   #'gnosis-dashboard-output-themata))

(defun gnosis-dashboard-nodes-show-isolated ()
  "Show isolated nodes (nodes with no connections at all).
Isolated nodes have no backlinks, no forward links, and no themata links."
  (interactive)
  (let* ((all-nodes-data (gnosis-nodes-get-nodes-data))
	 (all-ids (mapcar #'car all-nodes-data))
	 ;; Bulk fetch forward links (1 query instead of N)
	 (fwd-raw (when all-ids
		    (gnosis-select '[source dest] 'node-links
				   `(in source ,(vconcat all-ids)))))
	 (fwd-set (let ((h (make-hash-table :test 'equal)))
		    (dolist (link fwd-raw h)
		      (puthash (nth 0 link) t h))))
	 ;; Bulk fetch themata links (1 query instead of N)
	 (themata-raw (when all-ids
			(gnosis-select '[dest source] 'thema-links
				       `(in dest ,(vconcat all-ids)))))
	 (themata-set (let ((h (make-hash-table :test 'equal)))
			(dolist (link themata-raw h)
			  (puthash (nth 0 link) t h))))
         (isolated-ids (cl-loop for node in all-nodes-data
				for id = (nth 0 node)
				for backlink-count = (nth 2 node)
				when (and (= backlink-count 0)
					  (not (gethash id fwd-set))
					  (not (gethash id themata-set)))
				collect id)))
    (if isolated-ids
        (progn
          (gnosis-dashboard--push-current-view)
          (gnosis-dashboard-output-nodes isolated-ids))
      (message "No isolated nodes found"))))

(defun gnosis-dashboard-nodes-search-by-title (query)
  "Search ALL nodes by title for QUERY.
Searches the database for nodes whose titles contain the search term."
  (interactive "sSearch all nodes by title: ")
  (when (string-empty-p query)
    (user-error "Search query cannot be empty"))
  (let* ((all-nodes (gnosis-select '[id title] 'nodes))
         (matching-ids (cl-loop for node in all-nodes
				for id = (nth 0 node)
				for title = (nth 1 node)
				when (string-match-p (regexp-quote query) title)
				collect id)))
    (if matching-ids
        (progn
          ;; Save current view to history
          (gnosis-dashboard--push-current-view)
          (gnosis-dashboard-output-nodes matching-ids))
      (message "No nodes found with title matching '%s'" query))))

(defun gnosis-dashboard-nodes-filter-by-title (query)
  "Filter CURRENT nodes by title for QUERY.
Only searches within currently displayed nodes."
  (interactive "sFilter current nodes by title: ")
  (unless gnosis-dashboard-nodes-current-ids
    (user-error "No nodes to filter"))
  (when (string-empty-p query)
    (user-error "Search query cannot be empty"))
  (let* ((current-nodes
          (gnosis-select
           '[id title] 'nodes
           `(in id ,(vconcat
                     gnosis-dashboard-nodes-current-ids))))
         (matching-ids (cl-loop for node in current-nodes
				for id = (nth 0 node)
				for title = (nth 1 node)
				when (string-match-p (regexp-quote query) title)
				collect id)))
    (if matching-ids
        (progn
          ;; Save current view to history
          (gnosis-dashboard--push-current-view)
          (gnosis-dashboard-output-nodes matching-ids))
      (message "No nodes in current view match '%s'" query))))

(defun gnosis-dashboard-nodes-search-by-content (query)
  "Search all nodes for QUERY in files under `gnosis-nodes-dir'."
  (interactive "sSearch all nodes by content: ")
  (when (string-empty-p query)
    (user-error "Search query cannot be empty"))
  (let ((matching-ids (gnosis-nodes-search-content query)))
    (if matching-ids
        (progn
          (gnosis-dashboard--push-current-view)
          (gnosis-dashboard-output-nodes matching-ids))
      (message "No nodes found matching '%s'" query))))

(defun gnosis-dashboard-nodes-filter-by-content (query)
  "Filter current nodes by searching their files for QUERY."
  (interactive "sFilter current nodes by content: ")
  (unless gnosis-dashboard-nodes-current-ids
    (user-error "No nodes to filter"))
  (when (string-empty-p query)
    (user-error "Search query cannot be empty"))
  (let ((matching-ids (gnosis-nodes-search-content
                       query gnosis-dashboard-nodes-current-ids)))
    (if matching-ids
        (progn
          (gnosis-dashboard--push-current-view)
          (gnosis-dashboard-output-nodes matching-ids))
      (message "No nodes in current view match '%s'" query))))

(defun gnosis-dashboard-nodes-search-by-tag (tag)
  "Search ALL nodes by TAG."
  (interactive
   (list (completing-read "Search nodes by tag: "
                          (gnosis-nodes--all-tags)
                          nil t)))
  (when (string-empty-p tag)
    (user-error "Tag cannot be empty"))
  (let ((matching-ids (gnosis-nodes--nodes-by-tag tag)))
    (if matching-ids
        (progn
          (gnosis-dashboard--push-current-view)
          (gnosis-dashboard-output-nodes matching-ids))
      (message "No nodes found with tag '%s'" tag))))

(defun gnosis-dashboard-nodes-filter-by-tag (tag)
  "Filter CURRENT nodes by TAG."
  (interactive
   (list (completing-read "Filter nodes by tag: "
                          (gnosis-nodes--all-tags)
                          nil t)))
  (unless gnosis-dashboard-nodes-current-ids
    (user-error "No nodes to filter"))
  (when (string-empty-p tag)
    (user-error "Tag cannot be empty"))
  (let* ((nodes-with-tag (gnosis-nodes--nodes-by-tag tag))
         (matching-ids
          (cl-intersection
           gnosis-dashboard-nodes-current-ids
           nodes-with-tag :test #'equal)))
    (if matching-ids
        (progn
          (gnosis-dashboard--push-current-view)
          (gnosis-dashboard-output-nodes matching-ids))
      (message "No nodes in current view have tag '%s'" tag))))

(defun gnosis-dashboard-nodes-show-due ()
  "Show nodes linked to today's due themata."
  (interactive)
  (let* ((due-thema-ids (gnosis-review-get-due-themata))
         (node-ids (when due-thema-ids
                     (cl-remove-duplicates
                      (gnosis-select 'dest 'thema-links
                                     `(in source ,(vconcat due-thema-ids)) t)
                      :test #'equal))))
    (if node-ids
        (progn
          (gnosis-dashboard--push-current-view)
          (gnosis-dashboard-output-nodes node-ids))
      (message "No nodes linked to due themata"))))

(defun gnosis-dashboard-nodes-back ()
  "Go back to the previous nodes view, or to main dashboard if at top level."
  (interactive)
  (gnosis-dashboard--back))

(defun gnosis-dashboard-nodes-visit ()
  "Visit the node at point."
  (interactive)
  (gnosis-nodes-goto-id
   (or (tabulated-list-get-id) (user-error "No node at point"))))

(defun gnosis-dashboard-nodes-refresh ()
  "Refresh the current nodes view."
  (interactive)
  (gnosis-dashboard-output-nodes gnosis-dashboard-nodes-current-ids))

(defun gnosis-dashboard-nodes--sort-by (column &optional ascending)
  "Sort nodes dashboard by COLUMN.
If ASCENDING is non-nil, sort in ascending order, otherwise descending.
Moves cursor to the beginning of the buffer after sorting."
  (setq tabulated-list-sort-key (cons column (not ascending)))
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (goto-char (point-min)))

(keymap-popup-define gnosis-dashboard-nodes-sort-map
  "Sort Nodes"
  :description "Sort Nodes"
  :group "Sort By"
  "C-t" ("Title" (lambda () (interactive) (gnosis-dashboard-nodes--sort-by "Title" t)))
  "l" ("Links" (lambda () (interactive) (gnosis-dashboard-nodes--sort-by "Links")))
  "b" ("Backlinks" (lambda () (interactive) (gnosis-dashboard-nodes--sort-by "Backlinks")))
  "t" ("Themata" (lambda () (interactive) (gnosis-dashboard-nodes--sort-by "Themata"))))

(keymap-popup-define gnosis-dashboard-nodes-search-map
  "Search Nodes"
  :description "Search Nodes"
  :group "Search All Nodes"
  "C-t" ("By title" gnosis-dashboard-nodes-search-by-title)
  "c" ("By content" gnosis-dashboard-nodes-search-by-content)
  "t" ("By tag" gnosis-dashboard-nodes-search-by-tag))

(keymap-popup-define gnosis-dashboard-nodes-filter-map
  "Filter Nodes"
  :description "Filter Nodes"
  :group "Filter Current Nodes"
  "C-t" ("By title" gnosis-dashboard-nodes-filter-by-title)
  "c" ("By content" gnosis-dashboard-nodes-filter-by-content)
  "t" ("By tag" gnosis-dashboard-nodes-filter-by-tag))

(defun gnosis-dashboard-nodes-review ()
  "Review themata for node at point."
  (interactive)
  (gnosis-review-topic (tabulated-list-get-id)))

(defun gnosis-dashboard-nodes-review-with-depth ()
  "Review themata for node at point, prompting for link depths."
  (interactive)
  (let ((target (gnosis-review--session-target)))
    (gnosis-review-topic (tabulated-list-get-id)
		         (read-number "Forward link depth: " 1)
		         (read-number "Backlink depth: " 0)
                         target)))

(defun gnosis-dashboard-nodes-study ()
  "Open the study view for the node at point."
  (interactive)
  (gnosis-study-topic (or (tabulated-list-get-id) (user-error "No topic at point"))))

(defun gnosis-dashboard-nodes-practice ()
  "Practise marked nodes or the node at point, without rescheduling."
  (interactive)
  (gnosis-practice-topic (or gnosis-dashboard--selected-ids
                             (list (or (tabulated-list-get-id) (user-error "No topic at point"))))))

(defun gnosis-dashboard-nodes-due ()
  "Review due themata of marked nodes or the node at point."
  (interactive)
  (gnosis-review-due-topic (or gnosis-dashboard--selected-ids
                               (list (or (tabulated-list-get-id) (user-error "No topic at point"))))))

(keymap-popup-define gnosis-dashboard-nodes-mode-map
  "Nodes"
  :parent gnosis-dashboard-common-map
  :description (lambda ()
                 (format "Nodes (%s)"
                         (propertize
                          (number-to-string
                           (length gnosis-dashboard-nodes-current-ids))
                          'face 'font-lock-type-face)))
  :group "Navigate"
  "RET" ("Visit node" gnosis-dashboard-nodes-visit)
  "q" ("Back" gnosis-dashboard-nodes-back)
  "g" ("Refresh" gnosis-dashboard-nodes-refresh :stay-open t)
  :group "Search/Filter/Sort"
  "SPC" ("Search all..." :keymap gnosis-dashboard-nodes-search-map)
  "l" ("Filter current..." :keymap gnosis-dashboard-nodes-filter-map)
  "s" ("Sort..." :keymap gnosis-dashboard-nodes-sort-map)
  :group "View"
  "f" ("Show links" gnosis-dashboard-nodes-show-links)
  "b" ("Show backlinks" gnosis-dashboard-nodes-show-backlinks)
  "t" ("Show themata links" gnosis-dashboard-nodes-show-themata-links)
  "i" ("Show isolated" gnosis-dashboard-nodes-show-isolated)
  "d" ("Show due" gnosis-dashboard-nodes-show-due)
  :group "Study"
  "S" ("Topic study view" gnosis-dashboard-nodes-study)
  "r" ("Review due topics" gnosis-dashboard-nodes-due)
  "p" ("Practise topics" gnosis-dashboard-nodes-practice)
  :group "Review ahead"
  "a" ("Reschedule topic" gnosis-dashboard-nodes-review)
  "R" ("Reschedule with depth" gnosis-dashboard-nodes-review-with-depth))

(define-derived-mode gnosis-dashboard-nodes-mode
  tabulated-list-mode "Gnosis Nodes"
  "Major mode for gnosis dashboard nodes output."
  :keymap gnosis-dashboard-nodes-mode-map
  :interactive nil
  (gnosis-dashboard--common-setup))

(defun gnosis-dashboard-output-nodes (&optional node-ids)
  "Display nodes in dashboard.
If NODE-IDS is provided, display only those nodes.
Otherwise display all nodes.  Shows title, link count,
backlink count, and themata links count."
  (interactive)
  (pop-to-buffer-same-window gnosis-dashboard-buffer-name)
  (gnosis-dashboard-nodes-mode)
  (setf tabulated-list-format
        `[("Title" ,(/ (window-width) 2) t)
          ("Links" ,(/ (window-width) 8)
           gnosis-dashboard-sort-count)
          ("Backlinks" ,(/ (window-width) 8)
           gnosis-dashboard-sort-count)
          ("Themata" ,(/ (window-width) 8)
           gnosis-dashboard-sort-count)]
        tabulated-list-entries nil
        ;; Default sort based on user preferences.
        ;; tabulated-list uses FLIP where t=descending,
        ;; nil=ascending, so we invert the custom value.
        tabulated-list-sort-key
        (cons gnosis-dashboard-nodes-default-sort-column
              (not gnosis-dashboard-nodes-default-sort-ascending)))
  (make-local-variable 'tabulated-list-entries)
  (tabulated-list-init-header)
  (let* ((inhibit-read-only t)
         (entries (gnosis-dashboard-nodes--data node-ids))
         ;; Extract actual node IDs being displayed
         (displayed-ids (mapcar #'car entries)))
    (erase-buffer)
    (insert (format "Loading %s nodes..." (length entries)))
    (setq tabulated-list-entries entries)
    ;; Store current node IDs (now always populated)
    (setq gnosis-dashboard-nodes-current-ids displayed-ids)
    (tabulated-list-print t)))

(provide 'gnosis-dashboard)
;;; gnosis-dashboard.el ends here
