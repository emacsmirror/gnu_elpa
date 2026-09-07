;;; gnosis-test-graph-selection.el --- Graph traversal laws -*- lexical-binding: t; -*-

(require 'ert)
(require 'gnosis-links)
(require 'gnosis-test-helpers)

(defun gnosis-test-graph--nodes (ids)
  "Insert fixture nodes for IDS before indexing their links."
  (gnosis--insert-into 'nodes
                       (mapcar (lambda (id) (vector id "test.org" id "1" nil nil nil))
                               ids)))

(ert-deftest gnosis-test-graph-directional-frontiers ()
  "Apply each directional budget to the shared frontier, not separate walks."
  (gnosis-test-with-db
    (gnosis-test-graph--nodes '("a" "b" "c" "d" "e" "f" "x" "y"))
    (gnosis--insert-into
     'node-links '(["a" "b"] ["a" "c"] ["d" "a"] ["b" "e"]
                   ["d" "f"] ["x" "b"] ["y" "d"] ["e" "e"]))
    (let ((before (gnosis-select '* 'node-links)))
      (dolist (case '((nil nil ("a"))
                      (1 0 ("a" "b" "c"))
                      (0 1 ("a" "d"))
                      (1 1 ("a" "b" "c" "d"))
                      (2 1 ("a" "b" "c" "d" "e" "f"))
                      (1 2 ("a" "b" "c" "d" "x" "y"))))
        (should (equal (reverse (nth 2 case))
                       (gnosis-collect-nodes-at-depth "a" (car case) (cadr case)))))
      ;; Increasing either budget cannot remove a previously selected node.
      (dotimes (fwd 3)
        (dotimes (back 3)
          (let ((ids (gnosis-collect-nodes-at-depth "a" fwd back)))
            (should-not (seq-difference
                         ids (gnosis-collect-nodes-at-depth "a" (1+ fwd) back)))
            (should-not (seq-difference
                         ids (gnosis-collect-nodes-at-depth "a" fwd (1+ back)))))))
      (should (equal before (gnosis-select '* 'node-links))))))

(ert-deftest gnosis-test-graph-cycles-deduplication-and-bounds ()
  "Visit each cyclic node once, include isolated roots, and bound query work."
  (gnosis-test-with-db
    (gnosis-test-graph--nodes '("a" "b" "c"))
    (gnosis--insert-into 'node-links
                         '(["a" "b"] ["a" "c"] ["b" "c"] ["c" "a"] ["c" "c"]))
    (let ((select (symbol-function 'gnosis-select)) (calls 0))
      (cl-letf (((symbol-function 'gnosis-select)
                 (lambda (&rest args)
                   (cl-incf calls)
                   (apply select args))))
        (let ((ids (gnosis-collect-nodes-at-depth "a" 2000 2000)))
          (should (equal '("c" "b" "a") ids))
          (should (= (length ids) (length (delete-dups (copy-sequence ids)))))
          (should (= calls 4)))
        (setq calls 0)
        (should (equal '("isolated") (gnosis-collect-nodes-at-depth "isolated")))
        (should (zerop calls))
        (should (equal '("isolated") (gnosis-collect-nodes-at-depth "isolated" 2000 0)))
        (should (= calls 1))))))

(ert-deftest gnosis-test-graph-long-chain-bounded-stack ()
  "Traverse well beyond the Lisp recursion limit without recursive walking."
  (gnosis-test-with-db
    (gnosis-test-graph--nodes (mapcar #'number-to-string (number-sequence 0 200)))
    (gnosis--insert-into
     'node-links (cl-loop for n below 200 collect
                           (vector (number-to-string n) (number-to-string (1+ n)))))
    (let ((max-lisp-eval-depth 100))
      (should (equal (mapcar #'number-to-string (number-sequence 200 0 -1))
                     (gnosis-collect-nodes-at-depth "0" 200 0))))))

(provide 'gnosis-test-graph-selection)
;;; gnosis-test-graph-selection.el ends here
