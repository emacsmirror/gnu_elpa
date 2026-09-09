;;; gnosis-test-assets.el --- Managed asset invariants -*- lexical-binding: t; -*-
;;; Commentary:
;; Disposable files and connections only; pin the original scene digest.
;;; Code:
(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-model)

(defun gnosis-test-assets--source ()
  "Create and return a disposable directory with literal byte fixtures."
  (let ((directory (expand-file-name "source" gnosis-dir)))
    (make-directory directory t)
    (with-temp-file (expand-file-name "triangle.obj" directory)
      (insert "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n"))
    (with-temp-file (expand-file-name "scene.json" directory)
      (insert "{\"objects\":[{\"id\":\"triangle\",\"label\":\"Triangle\",\"path\":\"triangle.obj\"},{\"id\":\"other\",\"label\":\"Other triangle\",\"path\":\"triangle.obj\"}],\"initial_view\":[0,-90,1],\"license\":\"CC0; original test geometry\",\"source\":\"Gnosis ERT fixture\"}"))
    directory))

(ert-deftest gnosis-assets-model-pinned-pre-extraction-digest ()
  (gnosis-test-with-db
    (let* ((source (gnosis-test-assets--source))
           (file (expand-file-name "scene.json" source))
           ;; Measured by running gnosis-model-import at f1706baa before editing.
           (revision "f1411d34229679067f130fa88d630ae07a3afc6e9b80c9d733f0ea5924823bec"))
      (should (equal revision (gnosis-model--revision source)))
      (should (equal (concat revision "/scene.json") (gnosis-model-import file)))
      (should (gnosis-model-resolve (list (concat revision "/scene.json") "0" "0" "1")
                                    '("triangle"))))))

(ert-deftest gnosis-assets-revision-input-properties-and-explicit-files ()
  (gnosis-test-with-db
    (let* ((source (gnosis-test-assets--source))
           (names (list (propertize "triangle.obj" 'face 'bold) "scene.json" "triangle.obj"))
           (before (copy-tree names))
           (revision (gnosis-assets-revision source names)))
      (dolist (permutation (list names (reverse names) (append names names)))
        (let ((print-length 1) (print-level 1) (print-circle t))
          (should (equal revision (gnosis-assets-revision source permutation)))))
      (should (equal-including-properties names before))
      (with-temp-file (expand-file-name "unrelated" source) (insert "Not an asset"))
      (should (equal revision (gnosis-assets-revision source names)))
      (let ((file (expand-file-name "bytes" source))
            (coding-system-for-write 'no-conversion))
        (with-temp-file file (set-buffer-multibyte nil) (insert (unibyte-string 0 255 13 10)))
        (should (equal (secure-hash 'sha256 (unibyte-string 0 255 13 10))
                       (gnosis-assets-hash file)))))))

(ert-deftest gnosis-assets-connected-owner-and-legitimate-alias ()
  (gnosis-test-with-db
    (let* ((owner gnosis-db)
           (root (expand-file-name "assets" gnosis-dir))
           (alias (expand-file-name "alias" gnosis-dir)))
      (make-symbolic-link gnosis-dir alias)
      (let ((gnosis-dir "/unused-configuration"))
        (should (equal root (gnosis-assets-root owner))))
      (let ((gnosis-db (sqlite-open (expand-file-name "gnosis.db" alias))))
        (unwind-protect
            (progn
              (message "Alias PRAGMA database_list: %S" (sqlite-select gnosis-db "PRAGMA database_list"))
              (should (equal root (gnosis-assets-root)))
              (should-error (gnosis-assets-root owner) :type 'user-error))
          (sqlite-close gnosis-db)))
      (delete-file alias))))

(ert-deftest gnosis-assets-confined-paths-and-symlinks ()
  (gnosis-test-with-db
    (let* ((source (gnosis-test-assets--source))
           (root (gnosis-assets-root))
           (names '("triangle.obj"))
           (revision (gnosis-assets-revision source names)))
      (dolist (name '("../triangle.obj" "/triangle.obj" "." ".." "a/b" "a\\b" ""))
        (should-error (gnosis-assets-file source name)))
      (make-symbolic-link source root)
      (should-error (gnosis-assets-root))
      (should-error (gnosis-assets-import source names))
      (delete-file root)
      (make-directory root)
      (make-symbolic-link source (expand-file-name revision root))
      (should-error (gnosis-assets-validate root revision names))
      (should-error (gnosis-assets-import source names))
      (should (file-symlink-p (expand-file-name revision root)))
      (make-symbolic-link (expand-file-name "triangle.obj" source)
                          (expand-file-name "link.obj" source))
      (should-error (gnosis-assets-import source '("link.obj")))
      (should-error (gnosis-assets-hash (expand-file-name "link.obj" source)))
      (should-error (gnosis-assets-validate root "../source" names)))))

(ert-deftest gnosis-assets-retry-corruption-and-unrelated-preservation ()
  (gnosis-test-with-db
    (let* ((source (gnosis-test-assets--source))
           (names (list "triangle.obj"))
           (generated (list (cons "image.json" "{\"file\":\"triangle.obj\"}")))
           (before (copy-tree (list names generated)))
           (revision (gnosis-assets-import source names generated))
           (root (gnosis-assets-root))
           (other (gnosis-assets-import source '("scene.json")))
           (other-directory (expand-file-name other root))
           (file (expand-file-name "triangle.obj" (expand-file-name revision root))))
      (should (equal revision (gnosis-assets-import source names generated)))
      (should (equal before (list names generated)))
      (with-temp-file file (insert "Corrupt"))
      (should-error (gnosis-assets-import source names generated))
      (should (equal "Corrupt" (with-temp-buffer (insert-file-contents file) (buffer-string))))
      (should (equal other (gnosis-assets-revision other-directory '("scene.json")))))
    (should-not (directory-files (gnosis-assets-root) nil "^\\.import-"))))

(ert-deftest gnosis-assets-error-quit-retry-and-owner-change ()
  (gnosis-test-with-db
    (let* ((source (gnosis-test-assets--source))
           (root (gnosis-assets-root))
           (reference (gnosis-assets-import source '("scene.json")))
           (copy (symbol-function 'copy-file))
           (owner gnosis-db)
           (other (sqlite-open (expand-file-name "other.db" gnosis-dir))))
      (unwind-protect
          (progn
            (dolist (operation '(copy-file rename-file))
              (dolist (condition '(error quit))
                (cl-letf (((symbol-function operation)
                           (lambda (&rest _) (signal condition '("Injected interruption")))))
                  (should (eq condition
                              (condition-case err
                                  (gnosis-assets-import source '("triangle.obj"))
                                ((error quit) (car err))))))
                (should-not (directory-files root nil "^\\.import-"))
                (should (gnosis-assets-validate root reference '("scene.json")))))
            (cl-letf (((symbol-function 'copy-file)
                       (lambda (&rest args) (apply copy args) (setq gnosis-db other))))
              (should-error (gnosis-assets-import source '("triangle.obj"))))
            (setq gnosis-db owner)
            (should-not (directory-files root nil "^\\.import-"))
            (should (stringp (gnosis-assets-import source '("triangle.obj")))))
        (setq gnosis-db owner)
        (sqlite-close other)))))

(ert-deftest gnosis-assets-model-generated-manifest-size-limit ()
  (gnosis-test-with-db
    (let ((file (expand-file-name "scene.json" (gnosis-test-assets--source))))
      (should-error (gnosis-model-import file (make-string 65536 ?x)))
      (should-not (file-exists-p (gnosis-assets-root))))))

(provide 'gnosis-test-assets)
;;; gnosis-test-assets.el ends here
