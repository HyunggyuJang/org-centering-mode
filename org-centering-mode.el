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

(defvar org-centering--buffers nil
  "List of buffers in which `org-centering-mode' is activated.")

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
  (push (current-buffer) org-centering--buffers))

(defun org-centering--set-global-effects ()
  "Activate global effects."
  (advice-add #'org-display-inline-images :around #'+org-inlineimage-ensure-centering-a)
  (advice-add #'org--make-preview-overlay :after #'+org-latex-ensure-centering-a))

(defun org-centering--disable ()
  "Reset the current buffer to its normal appearance.
Also run the functions in `org-centering-global-effects' to undo
their effects if `org-centering-mode' is deactivated in the last
buffer in which it was active."
  ;; Restore global effects if necessary.
  (setq org-centering--buffers (delq (current-buffer) org-centering--buffers))
  (when (not org-centering--buffers)
    (org-centering--unset-global-effects)))

(defun org-centering--unset-global-effects ()
  "Deactivate global effects."
  (advice-remove #'org-display-inline-images #'+org-inlineimage-ensure-centering-a)
  (advice-remove #'org--make-preview-overlay #'+org-latex-ensure-centering-a))

;;;###autoload
(define-minor-mode org-centering-mode
  "Minor mode for centering org mode inline overlays."
  :init-value nil :lighter nil :global nil
  (+org-toggle-inlinefrags-center-h org-centering-mode)
  (if org-centering-mode
      (org-centering--enable)
    (org-centering--disable)))

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
