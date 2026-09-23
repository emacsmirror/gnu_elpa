;;; gnosis-test-campaign-model-numeric.el --- Point distances -*- lexical-binding: t; -*-

;;; Commentary:
;; Original-unit target radii, nearest selection and ties across model scales.

;;; Code:
(require 'ert)
(require 'gnosis-model)

(defun gnosis-test-model-numeric--target (id barycentric tolerance)
  "Return point target ID with BARYCENTRIC coordinates and TOLERANCE."
  `((id . ,id) (kind . "point") (mesh . "model") (face . 0)
    (barycentric . ,barycentric) (tolerance . ,tolerance)))

(defun gnosis-test-model-numeric--resolve (scale targets position verified)
  "Resolve POSITION at SCALE against TARGETS using VERIFIED or geometry."
  (let* ((triangle (mapcar (lambda (vertex)
                             (mapcar (lambda (n) (* n scale)) vertex))
                           '((0 0 0) (1 0 0) (0 1 0))))
         (geometry (list (cons "model" (vector triangle))))
         (scene (list '(version . 2) (cons 'targets targets)))
         (points (and verified
                      (mapcar (lambda (target)
                                (cons (alist-get 'id target)
                                      (gnosis-model--point target (vector triangle))))
                              targets))))
    (gnosis-model--candidate
     scene geometry (alist-get 'id (car targets))
     (list :mesh "model" :face 0
           :point (mapcar (lambda (n) (* n scale)) position))
     points)))

(ert-deftest gnosis-model-numeric-radius-original-units ()
  (dolist (scale '(1.0 1e-90 1e-200 1e-300))
    (dolist (verified '(nil t))
      (let ((targets (list (gnosis-test-model-numeric--target
                            "point" '(1 0 0) (* 0.25 scale)))))
        (dolist (position '((0 0 0) (0.125 0 0) (0.25 0 0)))
          (should (equal "point" (gnosis-test-model-numeric--resolve
                                  scale targets position verified))))
        (dolist (position '((0.5 0 0) (0.2 0.2 0) (0 0 0.5)))
          (should-not (gnosis-test-model-numeric--resolve
                       scale targets position verified)))))))

(ert-deftest gnosis-model-numeric-nearest-before-id ()
  (dolist (scale '(1.0 1e-90 1e-200 1e-300))
    (dolist (verified '(nil t))
      (let ((targets (list (gnosis-test-model-numeric--target
                            "a-far" '(1 0 0) scale)
                           (gnosis-test-model-numeric--target
                            "z-near" '(0 1 0) scale))))
        (dolist (order (list targets (reverse targets)))
          (should (equal "z-near" (gnosis-test-model-numeric--resolve
                                   scale order '(0.75 0 0) verified))))))))

(ert-deftest gnosis-model-numeric-equal-distance-id-tie ()
  (dolist (scale '(1.0 1e-90 1e-200 1e-300))
    (dolist (verified '(nil t))
      (let ((targets (list (gnosis-test-model-numeric--target
                            "z-left" '(1 0 0) scale)
                           (gnosis-test-model-numeric--target
                            "a-right" '(0 1 0) scale))))
        (dolist (order (list targets (reverse targets)))
          (should (equal "a-right" (gnosis-test-model-numeric--resolve
                                    scale order '(0.5 0 0) verified))))))))

(provide 'gnosis-test-campaign-model-numeric)
;;; gnosis-test-campaign-model-numeric.el ends here
