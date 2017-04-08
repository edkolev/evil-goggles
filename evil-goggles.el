;;; evil-goggles.el --- Add a visual hint to evil operations -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2017 edkolev

;; Author: edkolev <evgenysw@gmail.com>
;; URL: http://github.com/edkolev/evil-goggles
;; Package-Requires: ((emacs "24") (evil "1.0.0"))
;; Version: 0.0.1
;; Keywords: emulations, evil, vim, visual

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Add a visual hint to evil operations
;;
;; Usage:
;;
;; (evil-goggles-mode)
;;
;;; Code:

(defvar evil-goggles--on nil)
(defvar evil-goggles-show-for 0.200) ;; .100 or .200 seem best

(defcustom evil-goggles-faces-alist
  `(
    ( evil-delete . evil-ex-substitute-matches ) ;; isearch-fail
    ( evil-yank . evil-ex-substitute-replacement )
    )
  "Association list of faces to use for different commands")

(defcustom evil-goggles-default-face
  'region
  "Deafult face for the overlay")

(defun evil-goggles--face (command)
  (or
   (assoc-default command evil-goggles-faces-alist)
   evil-goggles-default-face))

(defun evil-goggles--show (beg end face)
  (let ((ov (evil-goggles--make-overlay beg end 'face face)))
    (sit-for evil-goggles-show-for)
    (delete-overlay ov)))

(defun evil-goggles--make-overlay (beg end &rest properties)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'priority 9999)
    (overlay-put ov 'window (selected-window))
    (while properties
      (overlay-put ov (pop properties) (pop properties)))
    ov))

(defun evil-goggles--show-p (beg end)
  (and (not evil-goggles--on)
       (not evil-inhibit-operator)
       (not evil-inhibit-operator-value)
       (numberp beg)
       (numberp end)
       (> (- end beg) 1)
       (<= (point-min) beg end)
       (>= (point-max) end beg)
       (not (evil-visual-state-p))
       (not (evil-insert-state-p))))

(defun evil-goggles--evil-delete-advice (orig-fun beg end &optional type register yank-handler)
  (evil-goggles--with-goggles beg end 'evil-delete
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type register yank-handler)))

(defun evil-goggles--evil-indent-advice (orig-fun beg end)
  (evil-goggles--with-goggles beg end 'evil-indent
    (evil-goggles--funcall-preserve-interactive orig-fun beg end)))

(defmacro evil-goggles--with-goggles (beg end adviced-fun &rest body)
  (declare (indent defun) (debug t))
  `(if (evil-goggles--show-p ,beg ,end)
         (let* ((evil-goggles--on t))
           (evil-goggles--show ,beg ,end (evil-goggles--face ,adviced-fun))
           (progn ,@body))
       (progn ,@body)))

(defmacro evil-goggles--funcall-preserve-interactive (orig-fun &rest args)
  `(if (called-interactively-p 'any)
       (funcall-interactively ,orig-fun ,@args)
     (funcall ,orig-fun ,@args)))

(defun evil-goggles--evil-yank-advice (orig-fun beg end &optional type register yank-handler)
  (evil-goggles--with-goggles beg end 'evil-yank
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type register yank-handler)))

(defun evil-goggles--evil-join-advice (orig-fun beg end)
  (let* ((beg-line (line-number-at-pos beg))
         (end-line (line-number-at-pos end))
         (line-count (- end-line beg-line)))
    (if (> line-count 1) ;; don't show goggles for single lines ("J"/"gJ" without count)
        (evil-goggles--with-goggles beg end 'evil-join
          (evil-goggles--funcall-preserve-interactive orig-fun beg end))
      (evil-goggles--funcall-preserve-interactive orig-fun beg end))))

(defun evil-goggles--evil-surround-region-advice (orig-fun beg end &optional type char force-new-line)
  (evil-goggles--with-goggles beg end 'evil-surround-region
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type char force-new-line)))

(defun evil-goggles--evil-commentary-advice (orig-fun beg end &optional type)
  (evil-goggles--with-goggles beg end 'evil-commentary
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type)))

(defun evil-goggles--evil-replace-with-register-advice (orig-fun count beg &optional end type register)
  (evil-goggles--with-goggles beg end 'evil-replace-with-register
    (evil-goggles--funcall-preserve-interactive orig-fun count beg end type register)))

(define-minor-mode evil-goggles-mode
  "evil-goggles global minor mode."
  :lighter " (⌐■-■)"
  :global t
  (cond
   (evil-goggles-mode
    (evil-goggles--advice-add-all))
   (t
    (evil-goggles--advice-remove-all)
    )))

(defvar evil-goggles--hooks (make-hash-table))

(defun evil-goggles--advice-add (fun advice-fun)
  (when evil-goggles-mode
    ;; clear any old advice
    (let ((old-advice-fun (gethash fun evil-goggles--hooks)))
      (when old-advice-fun
        (message "Replacing advice of %s" fun)
        (advice-remove fun old-advice-fun)))

    ;; add the new advice
    (advice-add fun :around advice-fun))

  ;; store the advice so it can be enabled/disabled by the mode
  (puthash fun advice-fun evil-goggles--hooks))

(defun evil-goggles--advice-add-all ()
  (maphash (lambda (advised-fun advice-fun) (advice-add advised-fun :around advice-fun)) evil-goggles--hooks))

(defun evil-goggles--advice-remove-all ()
  (maphash (lambda (advised-fun advice-fun) (advice-remove advised-fun advice-fun)) evil-goggles--hooks))

;; default advice-d core evil functions
(evil-goggles--advice-add 'evil-delete                   'evil-goggles--evil-delete-advice)
(evil-goggles--advice-add 'evil-indent                   'evil-goggles--evil-indent-advice)
(evil-goggles--advice-add 'evil-yank                     'evil-goggles--evil-yank-advice)
(evil-goggles--advice-add 'evil-join                     'evil-goggles--evil-join-advice)
(evil-goggles--advice-add 'evil-join-whitespace          'evil-goggles--evil-join-advice)
(evil-goggles--advice-add 'evil-surround-region          'evil-goggles--evil-surround-region-advice)
(evil-goggles--advice-add 'evil-commentary               'evil-goggles--evil-commentary-advice)
(evil-goggles--advice-add 'evil-replace-with-register    'evil-goggles--evil-replace-with-register-advice)

(provide 'evil-goggles)

;; evil-goggles.el end here
