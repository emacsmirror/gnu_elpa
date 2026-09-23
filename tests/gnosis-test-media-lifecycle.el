;;; gnosis-test-media-lifecycle.el --- Media owner regressions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Exercise insertion failure, recursive viewer retirement and native retry.

;;; Code:

(require 'gnosis-model-test-support)

(defun gnosis-test-media-lifecycle--require-canvas ()
  "Load the shipped optional renderer only for lifecycle execution."
  (let ((load-path (cons (gnosis-model--renderer-directory) load-path)))
    (require 'canvas-3d)))

(ert-deftest gnosis-media-lifecycle-model-native-successors ()
  (gnosis-test-media-lifecycle--require-canvas)
  (dolist (transition '(mode mode-roundtrip file file-roundtrip))
    (gnosis-test-with-db
     (save-window-excursion
       (let* ((resource (gnosis-model-import (gnosis-test-model--scene)))
              (owner (list :buffer (current-buffer) :mode major-mode
                           :tick (buffer-chars-modified-tick) :database gnosis-db))
              (retained (gnosis-test--add-basic-thema "Retained question" "Answer"))
              (changes (sqlite-select gnosis-db "SELECT total_changes()"))
              (draft (buffer-string))
              (successor (list :successor t))
              viewer process outer inner watchdog observed timed-out)
         (unwind-protect
             (cl-letf (((symbol-function 'gnosis-model-open)
                        (lambda (&rest _)
                          (setq viewer (generate-new-buffer " *model lifecycle*"))
                          (with-current-buffer viewer
                            (canvas-3d-mode)
                            (setq-local canvas-3d--process
                                        (make-pipe-process :name "model-lifecycle" :noquery t))
                            (setq process canvas-3d--process))
                          (pop-to-buffer viewer)
                          viewer))
                       ((symbol-function 'gnosis-model--canvas-size) (lambda () 400)))
               (setq outer
                     (run-at-time 0.02 nil
                                  (lambda ()
                                    (setq inner
                                          (run-at-time 0.02 nil
                                                       (lambda ()
                                                         (with-current-buffer viewer
                                                           (let ((context gnosis-model--author-context))
                                                             (pcase transition
                                                               ((or 'mode 'mode-roundtrip)
								(text-mode)
								(when (eq transition 'mode-roundtrip)
                                                                  (canvas-3d-mode)))
                                                               (_
								(set-visited-file-name
								 (expand-file-name "successor.txt" gnosis-dir) t)
								(when (eq transition 'file-roundtrip)
                                                                  (set-visited-file-name nil t))))
                                                             (let ((inhibit-read-only t))
                                                               (insert "Successor draft must survive"))
                                                             (setq-local gnosis-model--author-context
                                                                         (if (memq transition '(mode-roundtrip file-roundtrip))
                                                                             context successor)))
                                                           (setq observed (list (recursion-depth) major-mode
                                                                                buffer-file-name (buffer-string)))
                                                           (exit-recursive-edit)))))
                                    (recursive-edit)
                                    (exit-recursive-edit)))
                     watchdog (run-at-time 3 nil
                                           (lambda () (setq timed-out t) (abort-recursive-edit))))
               (should-error (gnosis-model--read-visual
                              (list resource "0" "0" "1") '("triangle") owner)
                             :type 'user-error)
               (should-not timed-out)
               (should (= (car observed) 2))
               (should (buffer-live-p viewer))
               (should-not (process-live-p process))
               (with-current-buffer viewer
                 (should (equal (cdr observed) (list major-mode buffer-file-name (buffer-string))))
                 (should (eq gnosis-model--author-context
                             (unless (memq transition '(mode-roundtrip file-roundtrip))
                               successor))))
               (should (gnosis-get 'id 'themata `(= id ,retained)))
               (should (equal changes (sqlite-select gnosis-db "SELECT total_changes()")))
               (should (equal draft (with-current-buffer (plist-get owner :buffer)
                                      (buffer-string))))
               (should-not (gnosis-select '* 'review-events))
               (should-not (gnosis-select '* 'practice-events)))
           (dolist (timer (list outer inner watchdog)) (when timer (cancel-timer timer)))
           (when (process-live-p process) (delete-process process))
           (when (buffer-live-p viewer)
             (with-current-buffer viewer
               (setq gnosis-model--author-context nil)
               (set-buffer-modified-p nil))
             (kill-buffer viewer))))))))

(ert-deftest gnosis-media-lifecycle-attachment-missing-canvas ()
  (gnosis-test-media-lifecycle--require-canvas)
  (with-temp-buffer
    (insert "Retained host")
    (let ((path (make-temp-file "canvas-capability-" nil ".obj")))
      (unwind-protect
          (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                    ((symbol-function 'image-type-available-p) (lambda (_) t))
                    ((symbol-function 'canvas-refresh) nil))
            (should (equal
                     (should-error (canvas-3d-attach path nil nil 128)
                                   :type 'user-error)
                     '(user-error "A graphical Emacs with canvas images is required")))
            (should (equal (buffer-string) "Retained host"))
            (should-not canvas-3d--process)
            (should-not canvas-3d--image))
        (delete-file path)))))

(ert-deftest gnosis-media-lifecycle-attachment-insertion-failure ()
  (gnosis-test-media-lifecycle--require-canvas)
  (dolist (condition '(error quit))
    (with-temp-buffer
      (text-mode)
      (insert "Retained host")
      (let* ((map (current-local-map))
             (logs (buffer-list))
             (path (make-temp-file "canvas-lifecycle-" nil ".obj"))
             (insertions 0)
             (refuse (lambda (&rest _)
                       (cl-incf insertions)
                       (signal condition '("Insertion refused")))))
        (unwind-protect
            (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                      ((symbol-function 'image-type-available-p) (lambda (_) t))
                      ;; Emacs 30 lacks this native canvas capability.
                      ((symbol-function 'canvas-refresh) #'ignore)
                      ((symbol-function 'canvas-3d--python) (lambda () "/usr/bin/python3"))
                      ((symbol-function 'make-process)
                       (lambda (&rest _) (make-pipe-process :name "canvas-lifecycle" :noquery t)))
                      ((symbol-function 'canvas-3d--request) #'ignore))
              (add-hook 'before-change-functions refuse nil t)
              (should (equal (list condition "Insertion refused")
                             (condition-case err
                                 (canvas-3d-attach path nil nil 128)
                               ((error quit) err))))
              (should (= insertions 1))
              (should-not canvas-3d--image)
              (should-not canvas-3d--process)
              (should-not canvas-3d--timer)
              (should-not (buffer-live-p canvas-3d--stderr))
              (should-not (seq-filter
                           (lambda (buffer)
                             (string-prefix-p " *Canvas 3D log*" (buffer-name buffer)))
                           (seq-difference (buffer-list) logs)))
              (should (equal (buffer-string) "Retained host"))
              (should (eq major-mode 'text-mode))
              (should (eq map (current-local-map)))
              (remove-hook 'before-change-functions refuse t)
              (canvas-3d-attach path nil nil 128)
              (should (process-live-p canvas-3d--process))
              (canvas-3d-detach)
              (should-not canvas-3d--image))
          (remove-hook 'before-change-functions refuse t)
          (canvas-3d-detach)
          (delete-file path))))))

(ert-deftest gnosis-media-lifecycle-graphical-retry-and-authoring ()
  (gnosis-test-media-lifecycle--require-canvas)
  (skip-unless (and (display-graphic-p) (image-type-available-p 'canvas)))
  (gnosis-test-with-db
   (save-window-excursion
     (let* ((scene (gnosis-test-model--scene))
            (reference (gnosis-model-import scene))
            (owner (list :buffer (current-buffer) :mode major-mode
                         :tick (buffer-chars-modified-tick) :database gnosis-db))
            (windows (current-window-configuration)))
       (save-window-excursion
         (with-temp-buffer
           (text-mode)
           (insert "Host text")
           (pop-to-buffer (current-buffer))
           (let ((refuse (lambda (&rest _) (error "Refused insertion")))
                 (map (current-local-map)))
             (unwind-protect
                 (progn
                   (add-hook 'before-change-functions refuse nil t)
                   (should-error (canvas-3d-attach scene nil nil 128))
                   (should-not canvas-3d--image)
                   (should (equal (buffer-string) "Host text"))
                   (remove-hook 'before-change-functions refuse t)
                   (canvas-3d-attach scene nil nil 128)
                   (let ((deadline (+ (float-time) 15)))
                     (while (and (not canvas-3d--frame) (< (float-time) deadline))
                       (accept-process-output nil 0.05)))
                   (should canvas-3d--frame)
                   (should (eq (plist-get canvas-3d--frame :owner) canvas-3d--process))
                   (redisplay t)
                   (should (eq major-mode 'text-mode))
                   (should (eq map (current-local-map))))
               (remove-hook 'before-change-functions refuse t)
               (canvas-3d-detach)))))
       (dolist (key '("RET" "q" "C-g"))
         (let (viewer process timer watchdog timed-out)
           (unwind-protect
               (progn
                 (setq timer
                       (run-at-time
                        0.05 0.05
                        (lambda ()
                          (when gnosis-model--author-context
                            (setq viewer (current-buffer) process canvas-3d--process)
                            (when (and canvas-3d--frame (not canvas-3d--busy)
                                       (not canvas-3d--dirty))
                              (cancel-timer timer)
                              (execute-kbd-macro (kbd key))))))
                       watchdog (run-at-time 20 nil
                                             (lambda () (setq timed-out t)
                                               (abort-recursive-edit))))
                 (let ((result
                        (condition-case nil
                            (gnosis-model--read-visual
                             (list reference "0" "0" "1") '("triangle")
                             (append owner (list :initial t)))
                          (quit 'cancelled))))
                   (should-not timed-out)
                   (if (equal key "RET")
                       (should (equal result (list (list reference "0" "0" "1.0") '("triangle"))))
                     (should (eq result 'cancelled)))
                   (should-not (buffer-live-p viewer))
                   (should-not (process-live-p process))))
             (when timer (cancel-timer timer))
             (when watchdog (cancel-timer watchdog))
             (when (buffer-live-p viewer) (kill-buffer viewer)))))
       (should (compare-window-configurations windows (current-window-configuration)))
       (should-not (gnosis-select '* 'themata))
       (should-not (gnosis-select '* 'review-events))
       (should-not (gnosis-select '* 'practice-events))))))

(provide 'gnosis-test-media-lifecycle)
;;; gnosis-test-media-lifecycle.el ends here
