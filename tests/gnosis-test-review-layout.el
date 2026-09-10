;;; gnosis-test-review-layout.el --- Review media layout -*- lexical-binding: t; -*-
;;; Commentary:
;; Display layout must not change encounter content or image identity.
;;; Code:
(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-review)

(ert-deftest gnosis-review-media-centered-and-resized ()
  (gnosis-test-with-db
   (save-window-excursion
     (let* ((buffer (gnosis-review--setup-buffer nil))
            (image (list 'image :type 'canvas :data-width 512 :data-height 512))
            (width 1000) (height 800))
       (unwind-protect
           (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) width))
                     ((symbol-function 'window-body-height) (lambda (&rest _) height))
                     ((symbol-function 'image-size)
                      (lambda (spec &rest _)
                        (cons (or (plist-get (cdr spec) :width) 512)
                              (or (plist-get (cdr spec) :height) 512)))))
             (switch-to-buffer buffer)
             (gnosis-display-keimenon (concat "Question\n\n" (propertize " " 'display image)))
             (goto-char (point-min))
             (search-forward "Question")
             (let ((position (point))
                   (text (buffer-substring-no-properties (point-min) (point-max))))
               (run-hooks 'window-configuration-change-hook)
               (should (= 480 (plist-get (cdr image) :width)))
               (let ((overlay (seq-find (lambda (o) (overlay-get o 'gnosis-review-media))
                                        (overlays-in (point-min) (point-max)))))
                 (should overlay)
                 (should (equal (get-text-property 0 'display (overlay-get overlay 'before-string))
                                '(space :align-to (- center (240))))))
               (setq width 300 height 400)
               (run-hooks 'window-configuration-change-hook)
               (should (= 240 (plist-get (cdr image) :width)))
               (should (= 512 (plist-get (cdr image) :data-width)))
               (should (eq image (get-text-property
                                  (text-property-any (point-min) (point-max) 'display image) 'display)))
               (should (= position (point)))
               (should (equal text (buffer-substring-no-properties (point-min) (point-max))))
               (should-not (string-match-p "Answer" text))
               (setq gnosis-center-content nil)
               (run-hooks 'window-configuration-change-hook)
               (should-not (seq-find (lambda (o) (overlay-get o 'gnosis-review-media))
                                     (overlays-in (point-min) (point-max))))))
         (kill-buffer buffer))))))

(ert-deftest gnosis-image-review-size-bounded-enlargement ()
  (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 1260))
            ((symbol-function 'window-body-height) (lambda (&rest _) 1300)))
    (should (equal (gnosis-image--display-size 297 332) '(594 . 664))))
  (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 300))
            ((symbol-function 'window-body-height) (lambda (&rest _) 400)))
    (should (equal (gnosis-image--display-size 1000 500) '(268 . 134)))))

(provide 'gnosis-test-review-layout)
;;; gnosis-test-review-layout.el ends here
