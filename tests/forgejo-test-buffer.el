;;; forgejo-test-buffer.el --- Tests for forgejo-buffer  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for shared display utilities: state formatting, label
;; colorization, relative time, login extraction, and EWOC node building.

;;; Code:

(require 'ert)
(require 'cl-lib)

(setq forgejo-markdown-mode 'text-mode)
(require 'forgejo-buffer)

;;; Group 1: State formatting

(ert-deftest forgejo-test-buffer-format-state-open ()
  "Open state uses the open face."
  (let ((result (forgejo-buffer--format-state "open")))
    (should (string= result "open"))
    (should (eq (get-text-property 0 'face result) 'forgejo-open-face))))

(ert-deftest forgejo-test-buffer-format-state-closed ()
  "Closed state uses the closed face."
  (let ((result (forgejo-buffer--format-state "closed")))
    (should (eq (get-text-property 0 'face result) 'forgejo-closed-face))))

;;; Group 2: Label formatting

(ert-deftest forgejo-test-buffer-format-labels ()
  "Labels are joined with commas and propertized with readable colors."
  (let* ((labels '(((name . "bug") (color . "d73a4a"))
                   ((name . "help") (color . "0075ca"))))
         (result (forgejo-buffer--format-labels labels)))
    (should (string-match-p "bug" result))
    (should (string-match-p "help" result))
    (should (string-match-p ", " result))
    (should (eq (plist-get (get-text-property 0 'face result) :weight) 'bold))
    (should (plist-get (get-text-property 0 'face result) :foreground))))

(ert-deftest forgejo-test-buffer-format-labels-empty ()
  "Empty labels return empty string."
  (should (string= (forgejo-buffer--format-labels nil) ""))
  (should (string= (forgejo-buffer--format-labels '()) "")))

;;; Group 3: Relative time

(ert-deftest forgejo-test-buffer-relative-time-nil ()
  "Nil or empty time returns empty string."
  (should (string= (forgejo-buffer--relative-time nil) ""))
  (should (string= (forgejo-buffer--relative-time "") "")))

(ert-deftest forgejo-test-buffer-relative-time-old ()
  "Very old time returns a date string."
  (let ((result (forgejo-buffer--relative-time "2020-01-01T00:00:00Z")))
    (should (string-match-p "2020-01-01" result))))

;;; Group 4: Login extraction

(ert-deftest forgejo-test-buffer-login ()
  "Extract login from user alist."
  (should (string= (forgejo-buffer--login '((login . "alice"))) "alice"))
  (should (null (forgejo-buffer--login :null)))
  (should (null (forgejo-buffer--login nil))))

;;; Group 5: EWOC node building

(ert-deftest forgejo-test-buffer-build-nodes ()
  "Build EWOC nodes from issue data and timeline."
  (let* ((issue '((number . 42) (title . "Test") (state . "open")
                  (body . "Description") (user . ((login . "alice")))
                  (labels) (milestone) (comments . 2)
                  (created_at . "2026-01-01T00:00:00Z")))
         (timeline `(((type . "comment") (body . "LGTM")
                      (user . ((login . "bob")))
                      (created_at . "2026-01-02T00:00:00Z"))
                     ((type . "close") (user . ((login . "alice")))
                      (created_at . "2026-01-03T00:00:00Z"))))
         (nodes (forgejo-buffer--build-nodes issue timeline)))
    (should (= (length nodes) 3))
    (should (eq (plist-get (nth 0 nodes) :type) 'header))
    (should (eq (plist-get (nth 1 nodes) :type) 'comment))
    (should (eq (plist-get (nth 2 nodes) :type) 'event))))

(ert-deftest forgejo-test-buffer-build-nodes-empty-timeline ()
  "Issue with no timeline produces only header node."
  (let* ((issue '((number . 1) (title . "Solo") (state . "open")
                  (body . "") (user . ((login . "me")))
                  (labels) (milestone) (comments . 0)
                  (created_at . "2026-01-01T00:00:00Z")))
         (nodes (forgejo-buffer--build-nodes issue nil)))
    (should (= (length nodes) 1))
    (should (eq (plist-get (car nodes) :type) 'header))))

;;; Group 6: Clean body

(ert-deftest forgejo-test-buffer-clean-body ()
  "Strip carriage returns, handle nil and :null."
  (should (string= (forgejo-buffer--clean-body "hello\r\nworld") "hello\nworld"))
  (should (null (forgejo-buffer--clean-body nil)))
  (should (null (forgejo-buffer--clean-body :null)))
  (should (null (forgejo-buffer--clean-body ""))))

;;; Group 7: Node keys

(ert-deftest forgejo-test-buffer-node-key-header ()
  (should (equal (forgejo-buffer--node-key '(:type header :number 1))
                 '(header))))

(ert-deftest forgejo-test-buffer-node-key-comment ()
  (should (equal (forgejo-buffer--node-key '(:type comment :id 42))
                 '(comment . 42))))

(ert-deftest forgejo-test-buffer-node-key-event ()
  (should (equal (forgejo-buffer--node-key
                  '(:type event :id 7 :event-type "closed"))
                 '(event . 7))))

(ert-deftest forgejo-test-buffer-node-key-review-link ()
  (should (equal (forgejo-buffer--node-key
                  '(:type review-link :review-id 99))
                 '(review-link . 99))))

;;; Group 8: Build-event-node stamps :id

(ert-deftest forgejo-test-buffer-build-event-node-stamps-id ()
  "Every non-header event node carries :id from the source event."
  (let* ((events '(((id . 11) (type . "close") (user . ((login . "a")))
                    (created_at . "2026-01-01T00:00:00Z"))
                   ((id . 12) (type . "label") (body . "1")
                    (label . ((name . "bug") (color . "ff0000")))
                    (user . ((login . "a")))
                    (created_at . "2026-01-02T00:00:00Z"))))
         (nodes (mapcar (lambda (e)
                          (forgejo-buffer--build-event-node e "a" events))
                        events)))
    (should (= 11 (plist-get (nth 0 nodes) :id)))
    (should (= 12 (plist-get (nth 1 nodes) :id)))))

;;; Group 9: Reactions

(ert-deftest forgejo-test-buffer-reaction-label-emoji-path ()
  "Known reactions use emoji labels when displayable."
  (cl-letf (((symbol-function 'forgejo-buffer--displayable-reaction-emoji-p)
             (lambda (_emoji) t)))
    (should (string= (forgejo-buffer--reaction-label "heart") "❤️"))))

(ert-deftest forgejo-test-buffer-reaction-label-fallback-path ()
  "Known reactions keep text labels when emoji is not displayable."
  (cl-letf (((symbol-function 'forgejo-buffer--displayable-reaction-emoji-p)
             (lambda (_emoji) nil)))
    (should (string= (forgejo-buffer--reaction-label "rocket") "rocket"))))

(ert-deftest forgejo-test-buffer-reaction-label-unknown ()
  "Unknown reactions keep their original content."
  (cl-letf (((symbol-function 'forgejo-buffer--displayable-reaction-emoji-p)
             (lambda (_emoji) nil)))
    (should (string= (forgejo-buffer--reaction-label "custom") "custom"))))

;;; Group 10: Reconcile

(defun forgejo-test-buffer--make-ewoc (nodes)
  "Build a fresh EWOC populated with NODES for testing reconcile."
  (with-current-buffer (generate-new-buffer " *fbtest*")
    (let ((ewoc (ewoc-create (lambda (data)
                               (insert (format "%S\n" data)))
                             nil nil t)))
      (dolist (n nodes)
        (ewoc-enter-last ewoc n))
      ewoc)))

(defun forgejo-test-buffer--ewoc-keys (ewoc)
  "Return the list of node keys currently in EWOC, in order."
  (let (keys (n (ewoc-nth ewoc 0)))
    (while n
      (push (forgejo-buffer--node-key (ewoc-data n)) keys)
      (setq n (ewoc-next ewoc n)))
    (nreverse keys)))

(ert-deftest forgejo-test-buffer-reconcile-noop ()
  "Reconciling identical node lists makes no changes."
  (let* ((nodes '((:type header :number 1 :title "t")
                  (:type comment :id 10 :body "a")
                  (:type event :id 11 :event-type "closed")))
         (ewoc (forgejo-test-buffer--make-ewoc nodes))
         (invalidated 0))
    (unwind-protect
        (cl-letf* ((orig (symbol-function 'ewoc-invalidate))
                   ((symbol-function 'ewoc-invalidate)
                    (lambda (&rest args) (cl-incf invalidated)
                      (apply orig args))))
          (forgejo-buffer--reconcile-ewoc ewoc (copy-tree nodes))
          (should (= invalidated 0))
          (should (equal (forgejo-test-buffer--ewoc-keys ewoc)
                         '((header) (comment . 10) (event . 11)))))
      (kill-buffer (ewoc-buffer ewoc)))))

(ert-deftest forgejo-test-buffer-reconcile-insert ()
  "Reconciling with a new comment inserts it at the right position."
  (let* ((old '((:type header :number 1)
                (:type comment :id 10 :body "first")
                (:type event :id 11 :event-type "closed")))
         (new '((:type header :number 1)
                (:type comment :id 10 :body "first")
                (:type comment :id 12 :body "new")
                (:type event :id 11 :event-type "closed")))
         (ewoc (forgejo-test-buffer--make-ewoc old)))
    (unwind-protect
        (progn
          (forgejo-buffer--reconcile-ewoc ewoc new)
          (should (equal (forgejo-test-buffer--ewoc-keys ewoc)
                         '((header) (comment . 10) (comment . 12)
                           (event . 11)))))
      (kill-buffer (ewoc-buffer ewoc)))))

(ert-deftest forgejo-test-buffer-reconcile-update ()
  "Reconciling a comment with changed body updates and invalidates it."
  (let* ((old '((:type comment :id 10 :body "old")))
         (new '((:type comment :id 10 :body "new")))
         (ewoc (forgejo-test-buffer--make-ewoc old))
         (invalidated 0))
    (unwind-protect
        (cl-letf* ((orig (symbol-function 'ewoc-invalidate))
                   ((symbol-function 'ewoc-invalidate)
                    (lambda (&rest args) (cl-incf invalidated)
                      (apply orig args))))
          (forgejo-buffer--reconcile-ewoc ewoc new)
          (should (= invalidated 1))
          (should (equal (plist-get (ewoc-data (ewoc-nth ewoc 0)) :body)
                         "new")))
      (kill-buffer (ewoc-buffer ewoc)))))

(ert-deftest forgejo-test-buffer-reconcile-delete ()
  "Reconciling with a node removed deletes it from the EWOC."
  (let* ((old '((:type comment :id 10 :body "a")
                (:type comment :id 11 :body "b")))
         (new '((:type comment :id 10 :body "a")))
         (ewoc (forgejo-test-buffer--make-ewoc old)))
    (unwind-protect
        (progn
          (forgejo-buffer--reconcile-ewoc ewoc new)
          (should (equal (forgejo-test-buffer--ewoc-keys ewoc)
                         '((comment . 10)))))
      (kill-buffer (ewoc-buffer ewoc)))))

(ert-deftest forgejo-test-buffer-reconcile-preserves-reactions ()
  "Reactions previously patched onto a node survive reconcile when the
new node doesn't carry :reactions."
  (let* ((old '((:type comment :id 10 :body "a"
                       :reactions (("heart" "alice")))))
         (new '((:type comment :id 10 :body "a")))
         (ewoc (forgejo-test-buffer--make-ewoc old)))
    (unwind-protect
        (progn
          (forgejo-buffer--reconcile-ewoc ewoc new)
          (should (equal (plist-get (ewoc-data (ewoc-nth ewoc 0)) :reactions)
                         '(("heart" "alice")))))
      (kill-buffer (ewoc-buffer ewoc)))))

;;; Group: Reference linkification

(defun forgejo-test-buffer--linkify (text)
  "Insert TEXT into a temp buffer and run `forgejo-buffer--linkify-refs'.
Return a list of (BEG END NUMBER REPO) for each ref found."
  (with-temp-buffer
    (insert text)
    (forgejo-buffer--linkify-refs (point-min) (point-max))
    (let ((pos (point-min)) refs)
      (while (< pos (point-max))
        (if-let* ((n (get-text-property pos 'forgejo-ref-number)))
            (let ((end (or (next-single-property-change
                            pos 'forgejo-ref-number)
                           (point-max))))
              (push (list pos end n
                          (get-text-property pos 'forgejo-ref-repo))
                    refs)
              (setq pos end))
          (setq pos (or (next-single-property-change
                         pos 'forgejo-ref-number)
                        (point-max)))))
      (nreverse refs))))

(ert-deftest forgejo-test-buffer-linkify-bare-hash ()
  "Bare #N is linkified."
  (let ((refs (forgejo-test-buffer--linkify "see #42 for context")))
    (should (= (length refs) 1))
    (should (= (nth 2 (car refs)) 42))
    (should (null (nth 3 (car refs))))))

(ert-deftest forgejo-test-buffer-linkify-bare-bang ()
  "Bare !N is linkified (Forgejo PR shorthand)."
  (let ((refs (forgejo-test-buffer--linkify "merged in !17 yesterday")))
    (should (= (length refs) 1))
    (should (= (nth 2 (car refs)) 17))
    (should (null (nth 3 (car refs))))))

(ert-deftest forgejo-test-buffer-linkify-qualified-hash ()
  "owner/repo#N is linkified with repo captured."
  (let ((refs (forgejo-test-buffer--linkify "guix/guix#8544 fixes it")))
    (should (= (length refs) 1))
    (should (= (nth 2 (car refs)) 8544))
    (should (string= (nth 3 (car refs)) "guix/guix"))))

(ert-deftest forgejo-test-buffer-linkify-qualified-bang ()
  "owner/repo!N is linkified with repo captured."
  (let ((refs (forgejo-test-buffer--linkify "see guix/guix!8641 for the PR")))
    (should (= (length refs) 1))
    (should (= (nth 2 (car refs)) 8641))
    (should (string= (nth 3 (car refs)) "guix/guix"))))

(ert-deftest forgejo-test-buffer-linkify-no-match-plain-text ()
  "Plain text without a #N or !N produces no refs."
  (should (null (forgejo-test-buffer--linkify "no references here"))))

(ert-deftest forgejo-test-buffer-linkify-multiple ()
  "Multiple refs in one body are all linkified."
  (let ((refs (forgejo-test-buffer--linkify "see #1 and guix/guix!2 and #3")))
    (should (= (length refs) 3))
    (should (equal (mapcar (lambda (r) (nth 2 r)) refs) '(1 2 3)))))

(provide 'forgejo-test-buffer)
;;; forgejo-test-buffer.el ends here
