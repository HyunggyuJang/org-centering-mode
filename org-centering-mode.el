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
;; Package-Requires: ((emacs "27.1"))
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

(defvar org-centering--buffers nil
  "List of buffers in which `org-centering-mode' is activated.")

(defvar org-centering--char-pixel-width
  (frame-char-width)
  "Character width in pixel size.")

(defun org-centering--kill-buffer-function ()
  "Disable `org-centering-mode' before killing a buffer, if necessary.
This function is for use in `kill-buffer-hook'.  It checks whether
`org-centering-mode' is enabled in the buffer to be killed and
adjusts `org-centering--buffers' and the global effects accordingly."
  (when org-centering-mode
    (setq org-centering--buffers (delq (current-buffer) org-centering--buffers))
    (when (not org-centering--buffers)
      (org-centering--unset-global-effects))))

(add-hook 'kill-buffer-hook #'org-centering--kill-buffer-function)

(defun org-centering--enable ()
  "Set up org-centering-mode for the current buffer.
Also run the functions in `org-centering-global-effects' if the
current buffer is the first buffer in which `org-centering-mode' is
activated."
  ;; Activate global effects.
  (when (not org-centering--buffers)
    (org-centering--set-global-effects))
  (push (current-buffer) org-centering--buffers)
  (org-centering-enable-inlinefrags))

(defun org-centering--set-global-effects ()
  "Activate global effects."
  (advice-add #'org-display-inline-images :around #'org-centering-ensure-inlineimage-centering-a)
  (advice-add #'org--make-preview-overlay :after #'org-centering-ensure-latex-centering-a))

(defun org-centering--disable ()
  "Reset the current buffer to its normal appearance.
Also run the functions in `org-centering-global-effects' to undo
their effects if `org-centering-mode' is deactivated in the last
buffer in which it was active."
  (org-centering-disable-inlinefrags)
  ;; Restore global effects if necessary.
  (setq org-centering--buffers (delq (current-buffer) org-centering--buffers))
  (when (not org-centering--buffers)
    (org-centering--unset-global-effects)))

(defun org-centering--unset-global-effects ()
  "Deactivate global effects."
  (advice-remove #'org-display-inline-images #'org-centering-ensure-inlineimage-centering-a)
  (advice-remove #'org--make-preview-overlay #'org-centering-ensure-latex-centering-a))

;;;###autoload
(define-minor-mode org-centering-mode
  "Minor mode for centering org mode inline overlays."
  :init-value nil :lighter nil :global nil
  (if org-centering-mode
      (org-centering--enable)
    (org-centering--disable)))

(defsubst org-centering--inlineimage-centering-internal (ov img)
  "Internal function for centering inline images."
      (let ((beg (overlay-start ov))
            offset
            width)
        (when (save-excursion
                (goto-char beg)
                (beginning-of-line 1)
                (setq offset (skip-syntax-forward " " (line-end-position)))
                (eq beg (point)))
          (setq width (car (image-size img 'pixel)))
          (if-let ((contingent-ov (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay)) (overlays-at (1+ (overlay-end ov))))))
              (setq width (+ width (car (image-size (overlay-get contingent-ov 'display) 'pixel)))))
          (setq offset (max (- (round (- (window-text-width nil 'pixel) width) (* 2 org-centering--char-pixel-width)) offset) 0))
          (overlay-put ov 'before-string (propertize (make-string offset ?  t) 'face 'org-latex-and-related)))))

(defun org-centering-ensure-inlineimage-centering-a (orig-fn &rest args)
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
                (if (eq prop 'display)
                    (org-centering--inlineimage-centering-internal ov img))
                (funcall overlay-put ov prop img))))
          (ignore overlay-put)
          (apply orig-fn args)))
    (apply orig-fn args)))

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

(defsubst org-centering--latex-centering-internal (ov beg)
  "Internal function for centering latex fragments."
  (let ((width (car (image-size (overlay-get ov 'display) 'pixel)))
        offset)
    (if (string-match org-centering-numbering-environments-regexp
                      (buffer-substring-no-properties
                       beg
                       (save-excursion (goto-char beg)
                                       (setq offset (- beg (point-at-bol)))
                                       (point-at-eol))))
        (setq width (- (* 2 width) org-centering--numbering-label-width)))
    (overlay-put ov 'before-string
                 (make-string (max 0
                                   (- (round (- (window-text-width nil 'pixel)
                                                width)
                                             (* 2 org-centering--char-pixel-width))
                                      offset))
                              ? ))))

(defun org-centering-ensure-latex-centering-a (beg end image &optional imagetype)
  "Assume to be used as an advice for `org--make-preview-overlay'.
As `org--make-preview-overlay' ensure to position point at BEG, we also rely this fact implicitly."
  (if org-centering-mode
      (unless (org-centering--inline-math? beg)
        (org-centering--latex-centering-internal
         (cl-find-if (lambda (o) (overlay-get o 'org-overlay-type)) (overlays-at beg))
         beg))))

(defun org-centering-enable-inlinefrags ()
  "Enable centering for existing inline fragments in current visible buffer."
  (dolist (ov (ignore-errors (overlays-in (point-min) (point-max))))
    (if (overlay-get ov 'org-image-overlay)
        (org-centering--inlineimage-centering-internal ov (overlay-get ov 'display))
      (if (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay)
          (let ((beg (overlay-start ov)))
            (unless (org-centering--inline-math? beg)
              (org-centering--latex-centering-internal ov beg)))))))

(defun org-centering-disable-inlinefrags ()
  "Disable centering for existing inline fragments in current visible buffer."
  (dolist (ov (ignore-errors (overlays-in (point-min) (point-max))))
    (if (or (overlay-get ov 'org-image-overlay)
            (and (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay)
                 (not (org-centering--inline-math? (overlay-start ov)))))
        (overlay-put ov 'before-string ""))))

(provide 'org-centering-mode)
;;; org-centering-mode.el ends here
