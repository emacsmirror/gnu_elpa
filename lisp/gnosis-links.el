;;; gnosis-links.el --- Link integrity and bulk link for gnosis  -*- lexical-binding: t; -*-

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

;;; Link extraction

(defun gnosis-extract-id-links (input &optional start)
  "Extract all link IDs from INPUT string and return them as a list.

START is the search starting position, used internally for recursion."
  (let ((start (or start 0)))
    (if (string-match "\\[\\[id:\\([^]]+\\)\\]\\[" input start)
        (cons (match-string 1 input)
              (gnosis-extract-id-links input (match-end 0)))
      nil)))

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

;;; Link integrity queries

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

;;; Link cleanup/sync

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

(provide 'gnosis-links)
;;; gnosis-links.el ends here
