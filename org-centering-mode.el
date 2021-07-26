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
  (if org-centering-mode
      (progn
        (add-function :around (local 'org-display-inline-images) #'org-centering-ensure-inlineimage-centering-a)
        (add-function :after (local 'org--make-preview-overlay) #'org-centering-ensure-latex-centering-a)
        (org-centering-enable-inlinefrags))
    (org-centering-disable-inlinefrags)
    (remove-function (local 'org-display-inline-images) #'org-centering-ensure-inlineimage-centering-a)
    (remove-function (local 'org--make-preview-overlay) #'org-centering-ensure-latex-centering-a)))

(defun org-centering-ensure-inlineimage-centering-a (orig-fn &rest args)
  (let
      ((overlay-put
        (symbol-function
         (function overlay-put))))
    (cl-letf
        (((symbol-function
           (function overlay-put))
          (lambda
            (ov prop img)
            (if (eq prop 'display)
                (let ((beg (overlay-start ov)))
                  (when (save-excursion (goto-char beg) (eq beg (point-at-bol)))
                    (let ((width (car (image-size img 'pixel))))
                      (if-let ((contingent-ov (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay)) (overlays-at (1+ (overlay-end ov))))))
                          (setq width (+ width (car (image-size (overlay-get contingent-ov 'display) 'pixel)))))
                      (let ((offset (max (round (- (window-text-width nil 'pixel) width) (* 2 7)) 0)))
                        (overlay-put ov 'before-string (propertize (make-string offset ?  t) 'face 'org-latex-and-related)))))))
            (funcall overlay-put ov prop img))))
      (ignore overlay-put)
      (apply orig-fn args)))
    (apply orig-fn args))

(defcustom org-centering-numbering-environments-regexp
  (regexp-opt (mapcar (lambda (env) (format "\\begin{%s}" env)) '("align" "equation" "gather")))
  "LaTeX fragments with numbering label.")

(defcustom org-centering--numbering-label-width
  1069
  "Full width of numbering label.")

(defsubst org-centering--inline-math? (beg)
  "Test whether BEG is a starting delimiter of inline math fragment."
  (or (eq (char-after beg) ?$)
      (eq (char-after (1+ beg)) ?\() ;assume first is \\
      ))

(defun org-centering-ensure-latex-centering-a (beg end image &optional imagetype)
  "Assume to be used as an advice for `org--make-preview-overlay'.
As `org--make-preview-overlay' ensure to position point at BEG, we also rely this fact implicitly."
  (unless (org-centering--inline-math? beg)
    (let* ((ov (cl-find-if (lambda (o) (overlay-get o 'org-overlay-type)) (overlays-at beg)))
           (img (overlay-get ov 'display))
           (img-width (car (image-size img 'pixel)))
           width)
      (if (string-match org-centering-numbering-environments-regexp
                        (buffer-substring-no-properties
                         beg
                         (point-at-eol)))
          (setq width (- (* 2 img-width) org-centering--numbering-label-width))
        (setq width img-width))
      (overlay-put ov 'before-string
                   (make-string (max 0
                                     (- (round (- (window-text-width nil 'pixel)
                                                  width)
                                               (* 2 7))
                                        (- beg (point-at-bol))))
                                ? )))))

(defun org-centering-enable-inlinefrags ()
  "Enable centering for existing inline fragments in current visible buffer."
  (dolist (ov (ignore-errors (overlays-in (point-min) (point-max))))
    (if (overlay-get ov 'org-image-overlay)
        (let ((beg (overlay-start ov)))
          (when (save-excursion (goto-char beg) (eq beg (point-at-bol)))
            (let* ((img (overlay-get ov 'display))
                   (width (car (image-size img 'pixel))))
              (if-let ((contingent-ov (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay)) (overlays-at (1+ (overlay-end ov))))))
                  (setq width (+ width (car (image-size (overlay-get contingent-ov 'display) 'pixel)))))
              (let ((offset (max (round (- (window-text-width nil 'pixel) width) (* 2 7)) 0)))
                (overlay-put ov 'before-string (propertize (make-string offset ?  t) 'face 'org-latex-and-related))))))
      (if (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay)
          (let ((beg (overlay-start ov)))
            (if (not (org-centering--inline-math? beg))
                (let* ((img (overlay-get ov 'display))
                       (img-width (car (image-size img 'pixel)))
                       width)
                  (if (string-match org-centering-numbering-environments-regexp
                                    (buffer-substring-no-properties
                                     beg
                                     (save-excursion (goto-char beg) (end-of-line) (point))))
                      (setq width  (- (* 2 img-width) org-centering--numbering-label-width))
                    (setq width img-width))
                  (overlay-put ov 'before-string
                               (make-string (max 0
                                                 (- (round (- (window-text-width nil 'pixel)
                                                              width)
                                                           (* 2 7))
                                                    (- beg (save-excursion (goto-char beg) (point-at-bol)))))
                                            ? )))))))))

(defun org-centering-disable-inlinefrags ()
  "Disable centering for existing inline fragments in current visible buffer."
  (dolist (ov (ignore-errors (overlays-in (point-min) (point-max))))
    (if (or (overlay-get ov 'org-image-overlay)
            (and (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay) (not (org-centering--inline-math? (overlay-start ov)))))
        (overlay-put ov 'before-string ""))))

(provide 'org-centering-mode)
;;; org-centering-mode.el ends here
