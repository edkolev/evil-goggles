;;; evil-goggles.el --- Add a visual hint to evil operations -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2017 edkolev

;; Author: edkolev <evgenysw@gmail.com>
;; URL: http://github.com/edkolev/evil-goggles
;; Package-Requires: ((emacs "25") (evil "1.0.0"))
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
;; Add a visual hint to evil edit operations such as yank, delete,
;; paste, etc.
;;
;; Usage:
;;
;; (evil-goggles-mode)
;;
;;; Code:

(require 'evil)
(require 'evil-goggles-faces)

(defcustom evil-goggles-duration 0.200
  "Time if floating seconds that the goggles overlay should last."
  :type 'number
  :group 'evil-goggles)

(defface evil-goggles-default-face
  '((t (:inherit region)))
  "Evil-goggles generic face."
  :group 'evil-goggles)

(defcustom evil-goggles-faces-alist nil
  "Association list of faces to use for different commands."
  :type 'boolean
  :group 'evil-goggles)

(defcustom evil-goggles-blacklist nil
  "List of functions which should not display the goggles overlay."
  :type 'boolean
  :group 'evil-goggles)

(defun evil-goggles--face (command)
  "Return the configured face for COMMAND, or the default face."
  (or
   (assoc-default command evil-goggles-faces-alist)
   'evil-goggles-default-face))

(defun evil-goggles--show (beg end face)
  "Show overlay in region from BEG to END with FACE."
  (let ((ov (evil-goggles--make-overlay beg end 'face face)))
    (unwind-protect
        (sit-for evil-goggles-duration)
      (delete-overlay ov))))

(defun evil-goggles--make-overlay (beg end &rest properties)
  "Make overlay in region from BEG to END with PROPERTIES."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'priority 9999)
    (overlay-put ov 'window (selected-window))
    (while properties
      (overlay-put ov (pop properties) (pop properties)))
    ov))

(defvar evil-goggles--on nil
  "When non-nil, the goggles overlay must not be displayed.

Used to prevent displaying multiple overlays for the same command.  For
example, when the user executes `evil-delete', the overlay should be
displayed, but when `evil-delete' calls internally `evil-yank', the
overlay must not be displayed.")

(defun evil-goggles--show-p (beg end)
  "Return t if the overlay should be displayed in region BEG to END."
  (and (not evil-goggles--on)
       (not evil-inhibit-operator-value)
       (bound-and-true-p evil-mode)
       (numberp beg)
       (numberp end)
       (> (- end beg) 1)
       (<= (point-min) beg end)
       (>= (point-max) end beg)
       (not (evil-visual-state-p))
       (not (evil-insert-state-p))
       (not (eq evil-this-type 'block))
       ;; don't show overlay when the region has nothing but whitespace
       (not (null (string-match-p "[^ \t\n]" (buffer-substring-no-properties beg end))))))

(defmacro evil-goggles--with-goggles (beg end overlay-face &rest body)
  "Show goggles overlay from BEG to END if the conditions are met.

OVERLAY-FACE is the face to use for the overlay.
The goggles overlay will be displayed briefly before BODY is executed.
BODY will be executed but an overlay will not be allowed to be
displayed while its running."
  (declare (indent defun) (debug t))
  `(if (evil-goggles--show-p ,beg ,end)
       (let* ((evil-goggles--on t))
         (evil-goggles--show ,beg ,end ,overlay-face)
         (progn ,@body))
     (progn ,@body)))

(defmacro evil-goggles--funcall-preserve-interactive (fun &rest args)
  "Call FUN with ARGS with `funcall' or `funcall-interactively'."
  `(if (called-interactively-p 'any)
       (funcall-interactively ,fun ,@args)
     (funcall ,fun ,@args)))

;;; core ends here ;;;

(defcustom evil-goggles-enable-delete t
  "If non-nil, enable delete support"
  :type 'boolean
  :group 'evil-goggles)

(defun evil-goggles--evil-delete-advice (orig-fun beg end &optional type register yank-handler)
  "Around-advice for function `evil-delete`.

ORIG-FUN is the original function.
BEG END &OPTIONAL TYPE REGISTER YANK-HANDLER are the arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-goggles-delete-face
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type register yank-handler)))

(defcustom evil-goggles-enable-indent t
  "If non-nil, enable indent support"
  :type 'boolean
  :group 'evil-goggles)

(defun evil-goggles--evil-indent-advice (orig-fun beg end)
  "Around-advice for function `evil-indent'.

ORIG-FUN is the original function.
BEG END are the arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-goggles-indent-face
    (evil-goggles--funcall-preserve-interactive orig-fun beg end)))

;;; mode defined below ;;;

(defcustom evil-goggles-lighter
  " EG"
  "String used on the mode-line."
  :group 'evil-goggles
  :type 'string)

;;;###autoload
(define-minor-mode evil-goggles-mode
  "evil-goggles global minor mode."
  :lighter evil-goggles-lighter
  :global t
  (cond
   (evil-goggles-mode

    ;; evil core functions
    (when evil-goggles-enable-delete
      (advice-add 'evil-delete :around 'evil-goggles--evil-delete-advice))
    (when evil-goggles-enable-indent
      (advice-add 'evil-indent :around 'evil-goggles--evil-indent-advice)))
   (t
    (advice-remove 'evil-delete 'evil-goggles--evil-delete-advice))))

(provide 'evil-goggles)

;;; evil-goggles.el ends here
