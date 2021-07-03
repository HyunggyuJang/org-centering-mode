;;; org-centering-mode.el --- Make inline image fragments centered in org mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Hyunggyu Jang
;;
;; Author: Hyunggyu Jang <https://github.com/hyunggyujang>
;; Maintainer: Hyunggyu Jang <murasakipurplez5@gmail.com>
;; Created: July 03, 2021
;; Modified: July 03, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/hyunggyujang/org-centering-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Make inline image fragments centered in org mode
;;
;;; Code:

(require 'cl-lib)
(require 'org)

;;;###autoload
(define-minor-mode org-centering-mode
  "Minor mode for centering org mode inline overlays."
  :init-value nil :lighter nil :global nil
  (+org-toggle-inlinefrags-center-h org-centering-mode)
  (if org-centering-mode
      (progn
        (advice-add #'org-display-inline-images :around #'+org-inlineimage-ensure-centering-a)
        (advice-add #'org--make-preview-overlay :after #'+org-latex-ensure-centering-a))
    (advice-remove #'org-display-inline-images #'+org-inlineimage-ensure-centering-a)
    (advice-remove #'org--make-preview-overlay #'+org-latex-ensure-centering-a)))

(defun +org-inlineimage-ensure-centering-a (orig-fn &rest args)
  (if org-centering-mode
      (let
          ((overlay-put
            (symbol-function
             (function overlay-put))))
        (cl-letf
            (((symbol-function
               (function overlay-put))
              (lambda
                (ov prop img)
                (if
                    (eq prop 'display)
                    (let*
                        ((width
                          (car
                           (image-size img 'pixel)))
                         (offset
                          (max
                           (floor
                            (/
                             (-
                              (window-text-width nil 'pixel)
                              width)
                             2))
                           0)))
                      (setq img
                            (cons
                             (car img)
                             `(:margin
                               (,offset . 0)
                               \,
                               (cdr img))))))
                (funcall overlay-put ov prop img))))
          (ignore overlay-put)
          (apply orig-fn args)))
    (apply orig-fn args)))

(defun +org-latex-ensure-centering-a (beg end image &optional imagetype)
  (if org-centering-mode
      (unless (or (eq (char-after beg) ?$)
                  (eq (char-after (1+ beg)) ?\() ;assume first is \\
                  )
        (let* ((ov (car (overlays-at beg)))
               (img (overlay-get ov 'display))
               (width (car (image-size img 'pixel)))
               (offset (max (floor (/ (- (window-text-width nil 'pixel) width) 2)) 0)))
          (setq img (cons (car img) `(:margin (,offset . 0) . ,(cdr img))))
          (overlay-put ov 'display img)))))

(defun +org-toggle-inlinefrags-center-h (enable?)
  (dolist (ov (cl-remove-if-not
               (lambda (o) (or
                       (overlay-get o 'org-image-overlay)
                       (and (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
                            (not (let ((beg (overlay-start o)))
                                   (or (eq (char-after beg) ?$)
                                       (eq (char-after (1+ beg)) ?\() ;assume first is \\
                                       ))))))
               (ignore-errors (overlays-in (point-min) (point-max)))))
    (let ((img (overlay-get ov 'display)))
      (if enable?
          (let* ((width (car (image-size img 'pixel)))
                 (offset (max (floor (/ (- (window-text-width nil 'pixel) width) 2)) 0)))
            (setq img (cons (car img) `(:margin (,offset . 0) . ,(cdr img)))))
        (if (eq (cadr img) :margin)
            (setq img (cons (car img) (cdddr img)))))
      (overlay-put ov 'display img))))

(provide 'org-centering-mode)
;;; org-centering-mode.el ends here
