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

(defcustom evil-goggles-show-for 0.200
  "Time if floating seconds that the goggles overlay should last."
  :type 'number)

(defcustom evil-goggles-default-face
  'region
  "Deafult face for the overlay.")

(defcustom evil-goggles-faces-alist nil
  "Association list of faces to use for different commands.")

(defcustom evil-goggles-blacklist nil
  "List of functions which should not display the goggles overlay.")

(defun evil-goggles--face (command)
  "Return the configured face for COMMAND, or the default face."
  (or
   (assoc-default command evil-goggles-faces-alist)
   evil-goggles-default-face))

(defun evil-goggles--show (beg end face)
  "Show overlay in region from BEG to END with FACE."
  (let ((ov (evil-goggles--make-overlay beg end 'face face)))
    (unwind-protect
        (sit-for evil-goggles-show-for)
      (delete-overlay ov))))

(defun evil-goggles--make-overlay (beg end &rest properties)
  "Make overlay in region from BEG to END with PROPERTIES."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'priority 9999)
    (overlay-put ov 'window (selected-window))
    (while properties
      (overlay-put ov (pop properties) (pop properties)))
    ov))

(defun evil-goggles--show-p (beg end)
  "Return t if the overlay should be displayed in region BEG to END."
  (and (not evil-goggles--on)
       (not evil-inhibit-operator-value)
       (numberp beg)
       (numberp end)
       (> (- end beg) 1)
       (<= (point-min) beg end)
       (>= (point-max) end beg)
       (not (evil-visual-state-p))
       (not (evil-insert-state-p))
       ;; don't show overlay when the region has nothing but whitespace
       (not (null (string-match-p "[^ \t\n]" (buffer-substring-no-properties beg end))))))

(defvar evil-goggles--on nil
  "When non-nil, the goggles overlay must not be displayed.

Used to prevent displaying multiple overlays for the same command.  For
example, when the user executes `evil-delete', the overlay should be
displayed, but when `evil-delete' calls internally `evil-yank', the
overlay must not be displayed.")

(defmacro evil-goggles--with-goggles (beg end adviced-fun &rest body)
  "Show goggles overlay from BEG to END if the conditions are met.

ADVICED-FUN is used to lookup which face should the overlay use.
The goggles overlay will be displayed briefly before BODY is executed.
BODY will be executed but an overlay will not be allowed to be
displayed while its running."
  (declare (indent defun) (debug t))
  `(if (evil-goggles--show-p ,beg ,end)
       (let* ((evil-goggles--on t))
         (evil-goggles--show ,beg ,end (evil-goggles--face ,adviced-fun))
         (progn ,@body))
     (progn ,@body)))

(defmacro evil-goggles--funcall-preserve-interactive (fun &rest args)
  "Call FUN with ARGS with `funcall' or `funcall-interactively'."
  `(if (called-interactively-p 'any)
       (funcall-interactively ,fun ,@args)
     (funcall ,fun ,@args)))

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

(defvar evil-goggles--advices (make-hash-table)
  "Hast table with functions which should be advice-d when evil goggles mode is toggled.")

(defun evil-goggles--advice-add (fun advice-fun)
  "Add advice around FUN with ADVICE-FUN.

Toggling evil goggles mode will add/remove the advice"
  (when evil-goggles-mode
    ;; clear any old advice
    (let ((old-advice-fun (gethash fun evil-goggles--advices)))
      (when old-advice-fun
        (advice-remove fun old-advice-fun)))

    ;; add the new advice
    (advice-add fun :around advice-fun))

  ;; store the advice so it can be enabled/disabled by the mode
  (puthash fun advice-fun evil-goggles--advices))

(defun evil-goggles--advice-add-all ()
  "Add advice around the functions registered in variable `evil-goggles--advices'."
  (maphash
   (lambda (advised-fun advice-fun)
     (unless (memq advised-fun evil-goggles-blacklist)
       (advice-add advised-fun :around advice-fun)))
   evil-goggles--advices))

(defun evil-goggles--advice-remove-all ()
  "Revemo advice around the functions registered in variable `evil-goggles--advices'."
  (maphash (lambda (advised-fun advice-fun) (advice-remove advised-fun advice-fun)) evil-goggles--advices))

;; advice-d core evil functions
(evil-goggles--advice-add 'evil-delete                'evil-goggles--evil-delete-advice)
(evil-goggles--advice-add 'evil-indent                'evil-goggles--evil-indent-advice)
(evil-goggles--advice-add 'evil-yank                  'evil-goggles--evil-yank-advice)
(evil-goggles--advice-add 'evil-join                  'evil-goggles--evil-join-advice)
(evil-goggles--advice-add 'evil-join-whitespace       'evil-goggles--evil-join-advice)
(evil-goggles--advice-add 'evil-paste-after           'evil-goggles--evil-paste-after-advice)
(evil-goggles--advice-add 'evil-paste-before          'evil-goggles--evil-paste-before-advice)

;; evil non-core packages
(evil-goggles--advice-add 'evil-surround-region       'evil-goggles--evil-surround-region-advice)
(evil-goggles--advice-add 'evil-commentary            'evil-goggles--evil-commentary-advice)
(evil-goggles--advice-add 'evil-replace-with-register 'evil-goggles--evil-replace-with-register-advice)
(evil-goggles--advice-add 'evil-ex-global             'evil-goggles--evil-ex-global-advice)

(defun evil-goggles--evil-delete-advice (orig-fun beg end &optional type register yank-handler)
  "Around-advice for function `evil-delete`.

ORIG-FUN is the original function.
BEG END &OPTIONAL TYPE REGISTER YANK-HANDLER are the arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-delete
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type register yank-handler)))

(defun evil-goggles--evil-indent-advice (orig-fun beg end)
  "Around-advice for function `evil-indent'.

ORIG-FUN is the original function.
BEG END are the arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-indent
    (evil-goggles--funcall-preserve-interactive orig-fun beg end)))

(defun evil-goggles--evil-yank-advice (orig-fun beg end &optional type register yank-handler)
  "Around-advice for function `evil-yank'.

ORIG-FUN is the original function.
BEG END &OPTIONAL TYPE REGISTER YANK-HANDLER are the arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-yank
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type register yank-handler)))

(defun evil-goggles--evil-join-advice (orig-fun beg end)
  "Around-advice for function `evil-join'.

ORIG-FUN is the original function.
BEG END are the arguments of the original function."
  (let* ((beg-line (line-number-at-pos beg))
         (end-line (line-number-at-pos end))
         (line-count (- end-line beg-line)))
    (if (> line-count 1) ;; don't show goggles for single lines ("J"/"gJ" without count)
        (evil-goggles--with-goggles beg end 'evil-join
          (evil-goggles--funcall-preserve-interactive orig-fun beg end))
      (evil-goggles--funcall-preserve-interactive orig-fun beg end))))

(defun evil-goggles--evil-surround-region-advice (orig-fun beg end &optional type char force-new-line)
  "Around-advice for function `evil-surround-region'.

ORIG-FUN is the original function.
BEG END &OPTIONAL TYPE CHAR FORCE-NEW-LINE are the arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-surround-region
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type char force-new-line)))

(defun evil-goggles--evil-commentary-advice (orig-fun beg end &optional type)
  "Around-advice for function `evil-commentary'.

ORIG-FUN is the original function.
BEG END &OPTIONAL TYPE are the arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-commentary
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type)))

(defun evil-goggles--evil-replace-with-register-advice (orig-fun count beg &optional end type register)
  "Around-advice for function `evil-replace-with-register'.

ORIG-FUN is the original function.
COUNT BEG &OPTIONAL END TYPE REGISTER are the arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-replace-with-register
    (evil-goggles--funcall-preserve-interactive orig-fun count beg end type register)))

(defun evil-goggles--evil-ex-global-advice (orig-fun beg end pattern command &optional invert)
  "Around-advice for function `evil-ex-global'.

ORIG-FUN is the original function.
BEG END PATTERN COMMAND &OPTIONAL INVERT are the arguments of the original function."
  (let* ((evil-goggles--on t)) ;; set to `t' to prevent showing the overlay
    (evil-goggles--funcall-preserve-interactive orig-fun beg end pattern command invert)))

(defun evil-goggles--evil-paste-after-advice (orig-fun count &optional register yank-handler)
  "Around-advice for function `evil-paste-after'.

ORIG-FUN is the original function.
COUNT REGISTER YANK-HANDLER are the arguments of the original function."
  (let ((was-in-normal-state (evil-normal-state-p))
        (orig-fun-result (evil-goggles--funcall-preserve-interactive orig-fun count register yank-handler)))
    (when was-in-normal-state
      (evil-goggles--evil-paste-show 'evil-paste-after))
    orig-fun-result))

(defun evil-goggles--evil-paste-before-advice (orig-fun count &optional register yank-handler)
  "Around-advice for function `evil-paste-before'.

ORIG-FUN is the original function.
COUNT REGISTER YANK-HANDLER are the arguments of the original function."
  (let ((was-in-normal-state (evil-normal-state-p))
        (orig-fun-result (evil-goggles--funcall-preserve-interactive orig-fun count register yank-handler)))
    (when was-in-normal-state
      (evil-goggles--evil-paste-show 'evil-paste-before))
    orig-fun-result))

(defun evil-goggles--evil-paste-show (adviced-fun)
  "Helper fun to show the goggles overlay on the last pasted text.

ADVICED-FUN is used to lookup the face for the overlay.
The overlay region is derermined by evil's variable `evil-last-paste'"
  (unless evil-goggles--on
    (let* ((beg (nth 3 evil-last-paste))
           (end (nth 4 evil-last-paste))
           (is-beg-at-eol (save-excursion (goto-char beg) (eolp)))
           (beg-corrected (if is-beg-at-eol (1+ beg) beg) ))
      (when (and beg end)
        (evil-goggles--show beg-corrected end (evil-goggles--face adviced-fun))))))

(provide 'evil-goggles)

;;; evil-goggles.el ends here
