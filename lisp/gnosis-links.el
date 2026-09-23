;;; gnosis-links.el --- Link integrity  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions

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

;; Link extraction, integrity checking, bulk linking, and cleanup.

;;; Code:

(require 'cl-lib)
(require 'gnosis-db)
(require 'gnosis-vc)
(require 'gnosis-utils)
(require 'gnosis-sqlite)

;; Runtime dependencies from gnosis.el (loaded before interactive use)
(declare-function gnosis-completing-read "gnosis")
(declare-function gnosis-collect-thema-ids "gnosis")
(declare-function gnosis-nodes--completion-candidates "gnosis-nodes")

;;; Link extraction

(defun gnosis-extract-id-links (input &optional start)
  "Extract bracketed Org ID links from INPUT in order.
Handle links with or without descriptions.  START is the optional
zero-based search starting position."
  (let ((start (or start 0)))
    (cl-loop while (string-match "\\[\\[id:\\([^]\n]+\\)\\]\\(?:\\[\\|\\]\\)"
                                 input start)
             collect (match-string 1 input)
             do (setq start (match-end 0)))))

;;; Node graph selection

(defun gnosis-collect-nodes-at-depth (node-id &optional fwd-depth back-depth)
  "Collect node IDs reachable from NODE-ID within depth limits.
FWD-DEPTH is max hops for forward links (default 0).
BACK-DEPTH is max hops for backlinks (default 0).
At each level, both enabled directions expand the same frontier.
A node reached through a backlink can thus be followed forward at the
next level, and vice versa, while that direction's budget permits.
Return a deduplicated list including NODE-ID itself, in the visited
hash table's key order."
  (let* ((fwd-depth (or fwd-depth 0))
	(back-depth (or back-depth 0))
	(max-depth (max fwd-depth back-depth))
	(visited (make-hash-table :test 'equal))
	(queue (list node-id)))
    (puthash node-id t visited)
    (dotimes (level max-depth)
      (when queue
	(let* ((qvec (vconcat queue))
	       (neighbors (append
			   (when (< level fwd-depth)
			     (gnosis-select 'dest 'node-links
					    `(in source ,qvec) t))
			   (when (< level back-depth)
			     (gnosis-select 'source 'node-links
					    `(in dest ,qvec) t))))
	       (next-queue nil))
	  (dolist (neighbor neighbors)
	    (unless (gethash neighbor visited)
	      (puthash neighbor t visited)
	      (push neighbor next-queue)))
	  (setq queue next-queue))))
    (hash-table-keys visited)))

;;; Bulk link operations

(defun gnosis--links-check-owner (db)
  "Refuse link mutations after the initiating connection DB is replaced."
  (unless (and (eq db gnosis-db)
               (condition-case nil (sqlite-select db "SELECT 1")
                 (error nil)))
    (user-error "Link database changed; restart the command")))

(defun gnosis--themata-to-update (themata string node-id)
  "Return pairs for THEMATA after replacing STRING with a link to NODE-ID."
  (cl-loop for thema in themata
           for thema-id = (nth 0 thema)
           for keimenon = (nth 1 thema)
           for result = (gnosis-utils-replace-string-with-link
                         keimenon string node-id)
           when (car result)
           collect (cons thema-id (cdr result))))

(defun gnosis--update-themata-keimenon (updates node-id)
  "Apply thema keimenon pairs to the database.
UPDATES contains (ID . NEW-KEIMENON) pairs.
NODE-ID is added to each updated thema's link index."
  (gnosis-sqlite-with-transaction (gnosis--ensure-db)
    (dolist (update updates)
      (gnosis-update 'themata
                     `(= keimenon ,(cdr update))
                     `(= id ,(car update)))
      (gnosis--insert-into
       'thema-links `([,(car update) ,node-id]) t))))

(defun gnosis--commit-bulk-link (count string)
  "Commit the bulk-link transaction for COUNT themata using STRING."
  (gnosis-vc--auto-commit
   (format "Bulk link: %d themata updated with %s" count string)))

(defun gnosis-bulk-link-themata (ids string node-id &optional validate-owner)
  "Replace STRING with a link to NODE-ID in themata with IDS.
Return the updated thema IDs.
When non-nil, call VALIDATE-OWNER with no arguments after confirmation,
immediately before writing.  It must signal an error if the caller's
initiating context is no longer valid; its return value is ignored."
  (when (string-empty-p string)
    (user-error "String cannot be empty"))
  (unless node-id
    (user-error "Node not found"))
  (let* ((db (gnosis--ensure-db))
         (themata (gnosis-select '[id keimenon] 'themata
                                 `(in id ,(vconcat ids))))
         (updates (gnosis--themata-to-update
                   themata string node-id)))
    (if (null updates)
        (progn
          (message "No themata to update for '%s'" string)
          nil)
      (when (y-or-n-p
             (format "Replace '%s' in %d themata? "
                     string (length updates)))
        (when validate-owner (funcall validate-owner))
        (gnosis--links-check-owner db)
        (gnosis-sqlite-with-transaction db
          (dolist (update updates)
            (unless (equal-including-properties
                     (gnosis-sqlite-select db
                                          "SELECT id, keimenon FROM themata WHERE id = ?"
                                          (list (car update)))
                     (list (assoc (car update) themata)))
              (user-error "Thema changed; restart bulk linking")))
          (gnosis--update-themata-keimenon updates node-id))
        (gnosis--commit-bulk-link (length updates) string)
        (message "Updated %d themata with links to '%s'"
                 (length updates) string)
        (mapcar #'car updates)))))

(defun gnosis-bulk-link-string (string node-id)
  "Replace all STRING instances in thema keimenon with a link to NODE-ID."
  (interactive
   (let* ((db (gnosis--ensure-db))
          (string (read-string "String to replace: "))
          (candidates
           (progn
             (gnosis--links-check-owner db)
             (require 'gnosis-nodes)
             (gnosis-nodes--completion-candidates
              (gnosis-select '[id title file] 'nodes))))
          (choice (gnosis-completing-read
                   "Select node: " (mapcar #'car candidates)))
          (node-id (cdr (assoc choice candidates))))
     (gnosis--links-check-owner db)
     (list string node-id)))
  (gnosis-bulk-link-themata
   (gnosis-collect-thema-ids :query string)
   string node-id))

;;; Link integrity queries

(defun gnosis--all-link-dests ()
  "Return all unique dest UUIDs from thema-links table."
  (let ((seen (make-hash-table :test 'equal)) result)
    ;; The old deduplicator kept the last occurrence.  Reverse a private
    ;; spine, then prepend unseen IDs to preserve that order in linear time.
    (dolist (dest (reverse (gnosis-select 'dest 'thema-links nil t)))
      (unless (gethash dest seen)
        (puthash dest t seen)
        (push dest result)))
    result))

(defun gnosis--all-node-ids ()
  "Return all node IDs from both nodes and journal tables."
  (append (gnosis-select 'id 'nodes nil t)
          (gnosis-select 'id 'journal nil t)))

(defun gnosis--orphaned-link-dests ()
  "Return thema-link destination UUIDs without a node or journal entry."
  (let ((link-dests (gnosis--all-link-dests))
        (node-set (make-hash-table :test 'equal)))
    (dolist (id (gnosis--all-node-ids))
      (puthash id t node-set))
    (seq-remove (lambda (dest) (gethash dest node-set)) link-dests)))

(defun gnosis--orphaned-links ()
  "Return (source dest) rows where dest has no matching node."
  (gnosis-sqlite-select
   (gnosis--ensure-db)
   (concat "SELECT source, dest FROM thema_links AS l "
           "WHERE NOT EXISTS (SELECT 1 FROM nodes WHERE id = l.dest) "
           "AND NOT EXISTS (SELECT 1 FROM journal WHERE id = l.dest)")))

(defun gnosis--node-links-missing-dest ()
  "Return node-link pairs whose destination has no matching node."
  (let* ((all-links (gnosis-select '[source dest]
                                   'node-links nil))
         (node-ids (gnosis--all-node-ids))
         (id-set (make-hash-table :test 'equal)))
    (dolist (id node-ids)
      (puthash id t id-set))
    (cl-loop for (source dest) in all-links
             unless (gethash dest id-set)
             collect (list source dest))))

(defun gnosis--node-links-missing-source ()
  "Return node-link pairs whose source has no matching node."
  (let* ((all-links (gnosis-select '[source dest]
                                   'node-links nil))
         (node-ids (gnosis--all-node-ids))
         (id-set (make-hash-table :test 'equal)))
    (dolist (id node-ids)
      (puthash id t id-set))
    (cl-loop for (source dest) in all-links
             unless (gethash source id-set)
             collect (list source dest))))

(defun gnosis--delete-broken-node-links (broken-links)
  "Delete BROKEN-LINKS pairs from the node-links table."
  (when broken-links
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (dolist (link broken-links)
        (gnosis-sqlite-execute
         (gnosis--ensure-db)
         "DELETE FROM node_links WHERE source IS ? AND dest IS ?"
         (list (car link) (cadr link)))))))

(defun gnosis--thema-expected-links (keimenon parathema)
  "Extract expected link IDs from KEIMENON and PARATHEMA."
  (cl-remove-duplicates
   (append (gnosis-extract-id-links keimenon)
           (gnosis-extract-id-links parathema))
   :test #'equal))

(defun gnosis--stale-links ()
  "Return (source dest) pairs in DB but not in thema text.
Fetches all themata, extras, and thema-links in bulk."
  (let* ((themata (gnosis-select '[id keimenon]
                                 'themata nil))
         (extras (gnosis-select '[id parathema]
                                'extras nil))
         (all-links (gnosis-select '[source dest]
                                   'thema-links nil))
         (themata-map (make-hash-table :test 'eql))
         (extras-map (make-hash-table :test 'equal))
         (expected-map (make-hash-table :test 'eql)))
    (dolist (thema themata)
      (puthash (car thema) (cadr thema) themata-map))
    (dolist (extra extras)
      (puthash (car extra) (cadr extra) extras-map))
    ;; Preserve link order, but look up and extract each source only once.
    (cl-loop
     for (source dest) in all-links
     for expected =
     (let ((cached (gethash source expected-map 'not-extracted)))
       (if (eq cached 'not-extracted)
           (puthash source
                    (gnosis--thema-expected-links
                     (or (gethash source themata-map) "")
                     (or (gethash source extras-map) ""))
                    expected-map)
         cached))
     unless (member dest expected)
     collect (list source dest))))

(defun gnosis--missing-links ()
  "Return (source dest) pairs in thema text but not in DB.
Fetches all themata, extras, and thema-links in bulk."
  (let* ((themata (gnosis-select '[id keimenon]
                                 'themata nil))
         (extras (gnosis-select '[id parathema]
                                'extras nil))
         (all-links (gnosis-select '[source dest]
                                   'thema-links nil))
         (extras-map (make-hash-table :test 'equal))
         (links-set (make-hash-table :test 'equal)))
    ;; Build extras lookup
    (dolist (extra extras)
      (puthash (car extra) (cadr extra) extras-map))
    ;; Build existing links set
    (dolist (link all-links)
      (puthash (list (car link) (cadr link))
               t links-set))
    ;; Find links in text that aren't in DB
    (cl-loop
     for (id keimenon) in themata
     for parathema = (gethash id extras-map "")
     for expected = (gnosis--thema-expected-links
                     (or keimenon "") (or parathema ""))
     append (cl-loop for dest in expected
                     for key = (list id dest)
                     unless (gethash key links-set)
                     collect (list id dest)))))

;;; Incremental link count

(defun gnosis--link-audit-revision (db)
  "Return DB's connection-local and external content change identity.
Conservatively invalidate on any write, including rolled-back writes.
Schema changes also invalidate the rowid cursor."
  (list (gnosis-sqlite-select db "SELECT total_changes()")
        (gnosis-sqlite-select db "PRAGMA data_version")
        (gnosis-sqlite-select db "PRAGMA schema_version")))

(defun gnosis--link-audit-new ()
  "Return a private, mutable builder for an incremental link issue count.
Only IDs, indexed links and counters survive a page; text is not retained."
  (list :tables '(nodes journal thema-links themata node-links)
        :after nil :count 0
        :nodes (make-hash-table :test 'equal)
        :links (make-hash-table :test 'equal)
        :indexed (make-hash-table :test 'equal)
        :orphans (make-hash-table :test 'equal)))

(defun gnosis--link-audit-page (db audit)
  "Accumulate at most 256 rows from DB into the private builder AUDIT.
Return non-nil when all tables have been scanned.  Use indexed rowid
pagination, including for the text/extras join, rather than fetching or
copying the whole collection before yielding.  The caller must reject the
builder if DB changes between pages; no transaction spans these calls."
  (let* ((table (car (plist-get audit :tables)))
         (after (plist-get audit :after))
         (nodes (plist-get audit :nodes))
         (links (plist-get audit :links))
         (indexed (plist-get audit :indexed))
         (orphans (plist-get audit :orphans))
         (count (plist-get audit :count))
         (rows
          (gnosis-sqlite-select
           db
           (concat
            (pcase-exhaustive table
              ('nodes "SELECT t.rowid, t.id FROM nodes t")
              ('journal "SELECT t.rowid, t.id FROM journal t")
              ('thema-links "SELECT t.rowid, t.source, t.dest FROM thema_links t")
              ('node-links "SELECT t.rowid, t.source, t.dest FROM node_links t")
              ('themata
               (concat "SELECT t.rowid, t.id, t.keimenon, e.parathema "
                       "FROM themata t LEFT JOIN extras e ON e.id = t.id")))
            (when after " WHERE t.rowid > ?")
            " ORDER BY t.rowid LIMIT 256")
           (when after (list after)))))
    (dolist (row rows)
      (setq after (car row))
      (pcase-exhaustive table
        ((or 'nodes 'journal) (puthash (nth 1 row) t nodes))
        ('thema-links
         (let ((pair (cdr row)) (dest (nth 2 row)))
           ;; Initially every index row is stale; matching text subtracts it.
           (cl-incf count)
           (puthash pair (1+ (gethash pair links 0)) links)
           (puthash pair t indexed)
           (unless (or (gethash dest nodes) (gethash dest orphans))
             (puthash dest t orphans)
             (cl-incf count))))
        ('themata
         (dolist (dest (gnosis--thema-expected-links
                       (or (nth 2 row) "") (or (nth 3 row) "")))
           (cl-decf count (gethash (list (nth 1 row) dest) links 0))
           (unless (gethash (list (nth 1 row) dest) indexed)
             (cl-incf count))))
        ('node-links
         (unless (gethash (nth 1 row) nodes) (cl-incf count))
         (unless (gethash (nth 2 row) nodes) (cl-incf count)))))
    (setf (plist-get audit :count) count
          (plist-get audit :after) after)
    (when (< (length rows) 256)
      (setf (plist-get audit :tables) (cdr (plist-get audit :tables))
            (plist-get audit :after) nil))
    (null (plist-get audit :tables))))

;;; Link report

(defun gnosis--links-check-format-count (n)
  "Format count N with face: green for 0, warning for >0."
  (propertize (number-to-string n)
              'face (if (zerop n) 'success 'warning)))

(defun gnosis--links-report-insert-heading (text)
  "Insert bold heading TEXT into current buffer."
  (insert (propertize text 'face 'bold) "\n"))

(defun gnosis--links-report-format-id (id)
  "Format ID with human-readable context.
String IDs get a node/journal title, integer IDs
get a keimenon excerpt."
  (cond
   ((stringp id)
    (let ((title (gnosis--links-report-node-title id)))
      (if title
          (format "%s (%s)" id
                  (truncate-string-to-width
                   title 50 nil nil "..."))
        (format "%s (deleted)" id))))
   ((integerp id)
    (let ((ctx (gnosis--links-report-thema-context id)))
      (if ctx
          (format "%s (%s)" id ctx)
        (format "%s" id))))
   (t (format "%s" id))))

(defun gnosis--links-report-insert-row (source dest)
  "Insert a link report row for SOURCE and DEST."
  (insert (format "  source: %s\n    dest: %s\n"
                  (gnosis--links-report-format-id source)
                  (gnosis--links-report-format-id dest))))

(defun gnosis--links-report-node-title (id)
  "Return the title for node ID, checking nodes then journal."
  (or (car (gnosis-select 'title 'nodes `(= id ,id) t))
      (car (gnosis-select 'title 'journal
                          `(= id ,id) t))))

(defun gnosis--links-report-thema-context (thema-id)
  "Return a short keimenon excerpt for THEMA-ID."
  (let ((keimenon (car (gnosis-select 'keimenon 'themata
                                      `(= id ,thema-id)
                                      t))))
    (when (and keimenon (stringp keimenon)
               (> (length keimenon) 0))
      (truncate-string-to-width
       keimenon 60 nil nil "..."))))

(defun gnosis--links-report-generate
    (orphaned stale missing
	      nl-missing-dest nl-missing-source)
  "Generate *Gnosis Link Report* buffer.
ORPHANED, STALE, MISSING are thema-links (source dest)
lists.  NL-MISSING-DEST, NL-MISSING-SOURCE are
node-links (source dest) lists."
  (with-current-buffer
      (get-buffer-create "*Gnosis Link Report*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (gnosis--links-report-insert-heading
       "Thema-links: orphaned dest")
      (if orphaned
          (dolist (row orphaned)
            (gnosis--links-report-insert-row
             (car row) (cadr row)))
        (insert "  None\n"))
      (insert "\n")
      (gnosis--links-report-insert-heading
       "Thema-links: stale (in DB but not in text)")
      (if stale
          (dolist (row stale)
            (gnosis--links-report-insert-row
             (car row) (cadr row)))
        (insert "  None\n"))
      (insert "\n")
      (gnosis--links-report-insert-heading
       "Thema-links: missing (in text but not in DB)")
      (if missing
          (dolist (row missing)
            (gnosis--links-report-insert-row
             (car row) (cadr row)))
        (insert "  None\n"))
      (insert "\n")
      (gnosis--links-report-insert-heading
       "Node-links: missing dest")
      (if nl-missing-dest
          (dolist (row nl-missing-dest)
            (gnosis--links-report-insert-row
             (car row) (cadr row)))
        (insert "  None\n"))
      (insert "\n")
      (gnosis--links-report-insert-heading
       "Node-links: missing source")
      (if nl-missing-source
          (dolist (row nl-missing-source)
            (gnosis--links-report-insert-row
             (car row) (cadr row)))
        (insert "  None\n"))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer (current-buffer))))

;;; Link cleanup/sync

;;;###autoload
(defun gnosis-links-check ()
  "Report link health for thema-links and node-links."
  (interactive)
  (let ((orphaned-dests (gnosis--orphaned-link-dests))
        (stale (gnosis--stale-links))
        (missing (gnosis--missing-links))
        (nl-missing-dest (gnosis--node-links-missing-dest))
        (nl-missing-source
         (gnosis--node-links-missing-source))
        (orphaned-rows nil))
    (let ((has-issues
           (or orphaned-dests stale missing
               nl-missing-dest nl-missing-source))
          (summary
           (format
            (concat
             "%s\n  thema-links:"
             " %s orphaned dest, %s stale,"
             " %s missing in DB"
             "\n  node-links:"
             " %s missing dest, %s missing source")
            (propertize "Link health:" 'face 'bold)
            (gnosis--links-check-format-count
             (length orphaned-dests))
            (gnosis--links-check-format-count
             (length stale))
            (gnosis--links-check-format-count
             (length missing))
            (gnosis--links-check-format-count
             (length nl-missing-dest))
            (gnosis--links-check-format-count
             (length nl-missing-source)))))
      (if (not has-issues)
          (message "%s" summary)
        (if (y-or-n-p "Issues found, view log? ")
            (progn
              (setq orphaned-rows
                    (gnosis--orphaned-links))
              (gnosis--links-report-generate
               orphaned-rows stale missing
               nl-missing-dest nl-missing-source))
          (message "%s" summary))))))

(defun gnosis--delete-orphaned-links (orphaned-dests)
  "Delete thema-links whose dest is in ORPHANED-DESTS."
  (when orphaned-dests
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (gnosis-sqlite-execute-batch
       (gnosis--ensure-db)
       "DELETE FROM thema_links WHERE dest IN (%s)"
       (remq nil orphaned-dests))
      (when (memq nil orphaned-dests)
        (gnosis-sqlite-execute
         (gnosis--ensure-db) "DELETE FROM thema_links WHERE dest IS NULL")))))

(defun gnosis--delete-stale-links (stale-links)
  "Delete STALE-LINKS from thema-links table.
Each element is a (source dest) pair."
  (when stale-links
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (dolist (link stale-links)
        (gnosis-sqlite-execute
         (gnosis--ensure-db)
         "DELETE FROM thema_links WHERE source IS ? AND dest IS ?"
         (list (car link) (cadr link)))))))

(defun gnosis--insert-missing-links (missing-links)
  "Insert MISSING-LINKS into thema-links table.
Each element is a (source dest) pair."
  (when missing-links
    (gnosis-sqlite-with-transaction (gnosis--ensure-db)
      (dolist (link missing-links)
        (gnosis--insert-into
         'thema-links
         `([,(car link) ,(cadr link)]))))))

(defun gnosis--commit-link-cleanup
    (orphaned stale missing
	      &optional node-links-removed)
  "Commit the link-cleanup transaction.
ORPHANED, STALE, MISSING are thema-links counts.
NODE-LINKS-REMOVED is the number of broken node-links
deleted."
  (gnosis-vc--auto-commit
   (format (concat "Link cleanup: thema-links %d orphaned, %d stale,"
                   " %d missing; node-links %d removed")
           orphaned stale missing (or node-links-removed 0))))

(defun gnosis--links-plan-state (db)
  "Return DB's exact source, index and node membership for link maintenance."
  (mapcar (lambda (sql) (sqlite-select db sql))
          '("SELECT id, keimenon FROM themata ORDER BY id"
            "SELECT id, parathema FROM extras ORDER BY id"
            "SELECT source, dest FROM thema_links ORDER BY source, dest"
            "SELECT source, dest FROM node_links ORDER BY source, dest"
            "SELECT id FROM nodes ORDER BY id"
            "SELECT id FROM journal ORDER BY id")))

(defun gnosis--links-maintain (sync)
  "Remove broken links; with SYNC, also insert links to available nodes.
Capture the plan and its owner before confirmation, without holding a
transaction across the prompt.  Refuse changes to any of its inputs."
  (let* ((db (gnosis--ensure-db))
         (state (gnosis--links-plan-state db))
         (orphaned (gnosis--orphaned-link-dests))
         (stale (gnosis--stale-links))
         (missing (and sync (gnosis--missing-links)))
         (available (and sync (gnosis--all-node-ids)))
         (insertable (seq-filter (lambda (link) (member (cadr link) available))
                                 missing))
         (broken (cl-remove-duplicates
                  (append (gnosis--node-links-missing-dest)
                          (gnosis--node-links-missing-source))
                  :test #'equal)))
    (if (not (or orphaned stale insertable broken))
        (message (cond ((not sync) "No broken links found")
                       (missing "%d missing links have unavailable targets")
                       (t "All links are in sync"))
                 (length missing))
      (when (y-or-n-p
             (format (concat "Remove %d orphaned + %d stale thema-links,"
                             " %d broken node-links%s? ")
                     (length orphaned) (length stale) (length broken)
                     (if sync (format ", add %d missing" (length insertable)) "")))
        (gnosis--links-check-owner db)
        (gnosis-sqlite-with-transaction db
          (unless (equal state (gnosis--links-plan-state db))
            (user-error "Link plan changed; restart the command"))
          (gnosis--delete-orphaned-links orphaned)
          (gnosis--delete-stale-links stale)
          (gnosis--delete-broken-node-links broken)
          (when sync
            (gnosis--insert-missing-links insertable)
            (setq missing (gnosis--missing-links))))
        (gnosis--commit-link-cleanup (length orphaned) (length stale)
                                    (length insertable) (length broken))
        (message (concat "Removed %d orphaned + %d stale thema-links,"
                         " %d broken node-links%s")
                 (length orphaned) (length stale) (length broken)
                 (if sync
                     (format "; added %d missing, %d unavailable targets remain"
                             (length insertable) (length missing))
                   ""))))))

;;;###autoload
(defun gnosis-links-cleanup ()
  "Remove orphaned or stale thema-links and broken node-links."
  (interactive)
  (gnosis--links-maintain nil))

;;;###autoload
(defun gnosis-links-sync ()
  "Remove broken links and insert missing links to known nodes or journals.
Authored links to unavailable targets remain unchanged and are reported."
  (interactive)
  (gnosis--links-maintain t))

(provide 'gnosis-links)
;;; gnosis-links.el ends here
