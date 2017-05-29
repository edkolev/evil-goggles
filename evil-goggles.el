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

(defcustom evil-goggles-duration 0.200
  "Time if floating seconds that the goggles overlay should last."
  :type 'number
  :group 'evil-goggles)

(defface evil-goggles-default-face
  '((t (:inherit region)))
  "Evil-goggles generic face."
  :group 'evil-goggles)

(defcustom evil-goggles-blacklist nil
  "List of functions which should not display the goggles overlay."
  :type 'boolean
  :group 'evil-goggles)

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

(defmacro evil-goggles--define-switch-and-face (switch-name switch-doc face-name face-doc)
  "Syntax sugar for defining a custom on/off variable and a custom face.

SWITCH-NAME is the name of the on/off variable.
SWITCH-DOC is the docstring for SWITCH-NAME.
FACE-NAME is the name of the custom face.
FACE-DOC is the docstring for FACE-NAME."
  (declare (indent 4) (debug t))
  `(progn
     (defcustom ,switch-name t
       ,(concat switch-doc "\nThis variable must be set before `evil-goggles-mode' is enabled")
       :type 'boolean
       :group 'evil-goggles)
     (defface ,face-name
       '((t (:inherit evil-goggles-default-face)))
       ,face-doc
       :group 'evil-goggles-faces)))

;;; core ends here ;;;

;; helper function to inherit from diff-mode's faces

(defun evil-goggles-use-diff-faces ()
  "Load `diff-mode' and use its faces for evil-goggles mode."
  (require 'diff-mode) ;; load diff-* faces
  (custom-set-faces
   '(evil-goggles-delete-face ((t (:inherit 'diff-removed))))
   '(evil-goggles-paste-face ((t (:inherit 'diff-added))))
   '(evil-goggles-yank-face ((t (:inherit 'diff-changed))))))

;; delete

(evil-goggles--define-switch-and-face
    evil-goggles-enable-delete "If non-nil, enable delete support"
    evil-goggles-delete-face "Face for delete action")

(defun evil-goggles--evil-delete-advice (orig-fun beg end &optional type register yank-handler)
  "Around-advice for function `evil-delete`.

ORIG-FUN is the original function.
BEG END &OPTIONAL TYPE REGISTER YANK-HANDLER are the arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-goggles-delete-face
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type register yank-handler)))

;; indent

(evil-goggles--define-switch-and-face
    evil-goggles-enable-indent "If non-nil, enable indent support"
    evil-goggles-indent-face "Face for indent action")

(defun evil-goggles--evil-indent-advice (orig-fun beg end)
  "Around-advice for function `evil-indent'.

ORIG-FUN is the original function.
BEG END are the arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-goggles-indent-face
    (evil-goggles--funcall-preserve-interactive orig-fun beg end)))

;; yank

(evil-goggles--define-switch-and-face
    evil-goggles-enable-yank "If non-nil, enable yank support"
    evil-goggles-yank-face "Face for yank action")

(defun evil-goggles--evil-yank-advice (orig-fun beg end &optional type register yank-handler)
  "Around-advice for function `evil-yank'.

ORIG-FUN is the original function.
BEG END &OPTIONAL TYPE REGISTER YANK-HANDLER are the arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-goggles-yank-face
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type register yank-handler)))

;; join

(evil-goggles--define-switch-and-face
    evil-goggles-enable-join "If non-nil, enable join support"
    evil-goggles-join-face "Face for join action")

(defun evil-goggles--evil-join-advice (orig-fun beg end)
  "Around-advice for function `evil-join'.

ORIG-FUN is the original function.
BEG END are the arguments of the original function."
  (let* ((beg-line (line-number-at-pos beg))
         (end-line (line-number-at-pos end))
         (line-count (- end-line beg-line)))
    (if (> line-count 1) ;; don't show goggles for single lines ("J"/"gJ" without count)
        (evil-goggles--with-goggles beg end 'evil-goggles-join-face
          (evil-goggles--funcall-preserve-interactive orig-fun beg end))
      (evil-goggles--funcall-preserve-interactive orig-fun beg end))))

;; indent (fill and move)

(evil-goggles--define-switch-and-face
    evil-goggles-enable-fill-and-move "If non-nil, enable fill and move (reformat) support"
    evil-goggles-fill-and-move-face "Face for fill and move (reformat) action")

(defun evil-goggles--evil-fill-and-move-advice (orig-fun beg end)
  "Around-advice for function `evil-fill-and-move'.

ORIG-FUN is the original function.
BEG END are arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-goggles-fill-and-move-face
    (evil-goggles--funcall-preserve-interactive orig-fun beg end)))

;; paste before and after

(evil-goggles--define-switch-and-face
    evil-goggles-enable-paste "If non-nil, enable paste support"
    evil-goggles-paste-face "Face for paste action")

(defun evil-goggles--evil-paste-after-advice (orig-fun count &optional register yank-handler)
  "Around-advice for function `evil-paste-after'.

ORIG-FUN is the original function.
COUNT REGISTER YANK-HANDLER are the arguments of the original function."
  (let ((was-in-normal-state (evil-normal-state-p))
        (orig-fun-result (evil-goggles--funcall-preserve-interactive orig-fun count register yank-handler)))
    (when was-in-normal-state
      (evil-goggles--evil-paste-show))
    orig-fun-result))

(defun evil-goggles--evil-paste-before-advice (orig-fun count &optional register yank-handler)
  "Around-advice for function `evil-paste-before'.

ORIG-FUN is the original function.
COUNT REGISTER YANK-HANDLER are the arguments of the original function."
  (let ((was-in-normal-state (evil-normal-state-p))
        (orig-fun-result (evil-goggles--funcall-preserve-interactive orig-fun count register yank-handler)))
    (when was-in-normal-state
      (evil-goggles--evil-paste-show))
    orig-fun-result))

(defun evil-goggles--evil-paste-show ()
  "Helper fun to show the goggles overlay on the last pasted text.

The overlay region is derermined by evil's variable `evil-last-paste'"
  (unless (or evil-goggles--on (null evil-last-paste))
    (let* ((beg (nth 3 evil-last-paste))
           (end (nth 4 evil-last-paste))
           (is-beg-at-eol (save-excursion (goto-char beg) (eolp)))
           (beg-corrected (if is-beg-at-eol (1+ beg) beg) ))
      (evil-goggles--show beg-corrected end 'evil-goggles-paste-face))))

;; shift left & right

(evil-goggles--define-switch-and-face
    evil-goggles-enable-shift "If non-nil, enable shift left/right support"
    evil-goggles-shift-face "Face for paste action")

(defun evil-goggles--evil-shift-advice (orig-fun beg end &optional count preserve-empty)
  "Around-advice for function `evil-shift-left` and `evil-shift-right`.

ORIG-FUN is the original function.
BEG END &OPTIONAL COUNT PRESERVE-EMPTY are the arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-goggles-shift-face
    (evil-goggles--funcall-preserve-interactive orig-fun beg end count preserve-empty)))

;; ex global

(defun evil-goggles--evil-ex-global-advice (orig-fun beg end pattern command &optional invert)
  "Around-advice for function `evil-ex-global'.

ORIG-FUN is the original function.
BEG END PATTERN COMMAND &OPTIONAL INVERT are the arguments of the original function."
  (let* ((evil-goggles--on t)) ;; set to `t' to prevent showing the overlay
    (evil-goggles--funcall-preserve-interactive orig-fun beg end pattern command invert)))

;; surround

(evil-goggles--define-switch-and-face
    evil-goggles-enable-surround "If non-nil, enable surround support"
    evil-goggles-surround-face "Face for surround action")

(defun evil-goggles--evil-surround-region-advice (orig-fun beg end &optional type char force-new-line)
  "Around-advice for function `evil-surround-region'.

ORIG-FUN is the original function.
BEG END &OPTIONAL TYPE CHAR FORCE-NEW-LINE are the arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-goggles-surround-face
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type char force-new-line)))

;; commentary

(evil-goggles--define-switch-and-face
    evil-goggles-enable-commentary "If non-nil, enable commentary support"
    evil-goggles-commentary-face "Face for commentary action")

(defun evil-goggles--evil-commentary-advice (orig-fun beg end &optional type)
  "Around-advice for function `evil-commentary'.

ORIG-FUN is the original function.
BEG END &OPTIONAL TYPE are the arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-goggles-commentary-face
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type)))

;; replace with register

(evil-goggles--define-switch-and-face
    evil-goggles-enable-replace-with-register "If non-nil, enable replace with register support"
    evil-goggles-replace-with-register-face "Face for replace with register action")

(defun evil-goggles--evil-replace-with-register-advice (orig-fun count beg &optional end type register)
  "Around-advice for function `evil-replace-with-register'.

ORIG-FUN is the original function.
COUNT BEG &OPTIONAL END TYPE REGISTER are the arguments of the original function."
  (evil-goggles--with-goggles beg end 'evil-goggles-replace-with-register-face
    (evil-goggles--funcall-preserve-interactive orig-fun count beg end type register)))

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
      (advice-add 'evil-indent :around 'evil-goggles--evil-indent-advice))

    (when evil-goggles-enable-yank
      (advice-add 'evil-yank :around 'evil-goggles--evil-yank-advice))

    (when evil-goggles-enable-join
      (advice-add 'evil-join :around 'evil-goggles--evil-join-advice)
      (advice-add 'evil-join-whitespace :around 'evil-goggles--evil-join-advice))

    (when evil-goggles-enable-fill-and-move
      (advice-add 'evil-fill-and-move :around 'evil-goggles--evil-fill-and-move-advice))

    (when evil-goggles-enable-paste
      (advice-add 'evil-paste-after :around 'evil-goggles--evil-paste-after-advice)
      (advice-add 'evil-paste-before :around 'evil-goggles--evil-paste-before-advice))

    (when evil-goggles-enable-shift
      (advice-add 'evil-shift-left :around 'evil-goggles--evil-shift-advice)
      (advice-add 'evil-shift-right :around 'evil-goggles--evil-shift-advice))

    ;; make sure :global and :v don't show the goggles overlay
    (advice-add 'evil-ex-global :around 'evil-goggles--evil-ex-global-advice)

    ;; evil non-core functions

    (when evil-goggles-enable-surround
      (advice-add 'evil-surround-region :around 'evil-goggles--evil-surround-region-advice))

    (when evil-goggles-enable-commentary
      (advice-add 'evil-commentary :around 'evil-goggles--evil-commentary-advice))

    (when evil-goggles-enable-replace-with-register
      (advice-add 'evil-replace-with-register :around 'evil-goggles--evil-replace-with-register-advice)))
   (t
    (advice-remove 'evil-delete 'evil-goggles--evil-delete-advice)
    (advice-remove 'evil-indent 'evil-goggles--evil-indent-advice)
    (advice-remove 'evil-yank 'evil-goggles--evil-yank-advice)
    (advice-remove 'evil-join 'evil-goggles--evil-join-advice)
    (advice-remove 'evil-join-whitespace 'evil-goggles--evil-join-advice)
    (advice-remove 'evil-fill-and-move 'evil-goggles--evil-fill-and-move-advice)
    (advice-remove 'evil-paste-after 'evil-goggles--evil-paste-after-advice)
    (advice-remove 'evil-paste-before 'evil-goggles--evil-paste-before-advice)
    (advice-remove 'evil-shift-left 'evil-goggles--evil-shift-advice)
    (advice-remove 'evil-shift-right 'evil-goggles--evil-shift-advice)

    (advice-remove 'evil-ex-global 'evil-goggles--evil-ex-global-advice)

    ;; evil non-core functions
    (advice-remove 'evil-surround-region 'evil-goggles--evil-surround-region-advice)
    (advice-remove 'evil-commentary 'evil-goggles--evil-commentary-advice)
    (advice-remove 'evil-replace-with-register 'evil-goggles--evil-replace-with-register-advice))))

(provide 'evil-goggles)

;;; evil-goggles.el ends here
