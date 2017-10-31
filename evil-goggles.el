;;; evil-goggles.el --- Add a visual hint to evil operations -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2017 edkolev

;; Author: edkolev <evgenysw@gmail.com>
;; URL: http://github.com/edkolev/evil-goggles
;; Package-Requires: ((emacs "24.4") (evil "1.0.0"))
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
(require 'cl-lib)

(defcustom evil-goggles-duration 0.200
  "Time in floating seconds that the goggles overlay should last.

This affects the hints which are displayed before the operation, such
as when deleting."
  :type 'number
  :group 'evil-goggles)

;; TODO better name
(defcustom evil-goggles-duration2 0.400
  "Time in floating seconds that the goggles overlay should last.

This affects the hints which are displayed before the operation"
  :type 'number
  :group 'evil-goggles)

(defcustom evil-goggles-pulse nil
  "If t, the overlay hint will pulse rather than appear and disapper.

This option is experimental."
  :type 'boolean
  :group 'evil-goggles)

(defface evil-goggles-default-face
  '((t (:inherit region)))
  "Evil-goggles default face."
  :group 'evil-goggles)

(defface evil-goggles--pulse-face nil
  "Temporary face used when pulsing, should not be customized.

This is needed because the pulse package expects to receive a face, it
can't work with input such as (backgound . \"red\")."
  :group 'evil-goggles)

(autoload 'pulse-momentary-highlight-overlay "pulse")

(defun evil-goggles--pulse-overlay (ov background dur)
  "Pulse the overlay OV with the BACKGROUND color for DUR duration.

This function returns immediately, it doesn't wait for the pulse
animation to end."
  (let* ((pulse-delay 0.03)
         (pulse-iterations (round dur pulse-delay)))
    (ignore pulse-iterations) ;; silence compile warning Unused lexical variable
    (set-face-attribute 'evil-goggles--pulse-face nil :background background)
    (pulse-momentary-highlight-overlay ov 'evil-goggles--pulse-face)))

(defun evil-goggles--face-background (face)
  "Return the background of FACE or use a fallback.

If the given FACE doesn't have a background, then fallback to the
background of 'evil-goggles-default-face, then 'region."
  (face-background face nil '(evil-goggles-default-face region)))

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
overlay must not be re-displayed.")

(defun evil-goggles--show-p (beg end)
  "Return t if the overlay should be displayed in region BEG to END."
  (and (not evil-inhibit-operator-value)
       (bound-and-true-p evil-mode)
       (numberp beg)
       (numberp end)
       (> (- end beg) 1)
       (<= (point-min) beg end)
       (>= (point-max) end beg)
       (not (evil-visual-state-p))
       (not (evil-insert-state-p))
       ;; don't show overlay when evil-mc has multiple fake cursors
       (not (and (fboundp 'evil-mc-has-cursors-p) (evil-mc-has-cursors-p)))
       ;; don't show overlay when the region has nothing but whitespace
       (not (null (string-match-p "[^ \t\n]" (buffer-substring-no-properties beg end))))))

(defun evil-goggles--overlay-insert-behind-hook (ov afterp beg end &optional len)
  "Function which grows/shriks the overlay OV when its text changes.

The OV, AFTERP, BEG, END, LEN arguments are specified by the calling
convention for the insert-behind-hooks overlay property."
  (when afterp
    (if (zerop len)
        (progn
          (setq len (- end beg))
          (move-overlay ov (overlay-start ov) (+ len (overlay-end ov))))
      (move-overlay ov (overlay-start ov) (- (overlay-end ov) len) ))))

(defmacro evil-goggles--with-post-hint (beg end face dur &rest body)
  "Show hint from BEG to END with face FACE for DUR seconds.

BODY is executed after the hint is displayed but before it's
removed.  As a result any changes BODY does on the text will be
visualized by the hint."
  (declare (indent 4) (debug t))
  `(evil-goggles--if-hint-on ,beg ,end (progn ,@body)
     (evil-goggles--show-overlay ,beg ,end ,face (or ,dur evil-goggles-duration2)
       ,@body)))

(defun evil-goggles--show-or-pulse-overlay (ov face dur)
  "Show or pulse overlay OV with face FACE.

DUR is used only when pulsing.
The overlay is pulsed if variable `evil-goggles-pulse' is t."
  (if evil-goggles-pulse
      (evil-goggles--pulse-overlay ov (evil-goggles--face-background face) dur)
    (overlay-put ov 'face face)))

(defmacro evil-goggles--if-hint-on (beg end body1 &rest body2)
  "Run one block of code if hint is visible, run the other if not.

If hint is visible, check it's ok to display it from BEG to END.  If
it's not, do BODY1, else BODY2."
  ;; TODO indent like `if'
  (declare (indent 3) (debug t))
  `(if (and (not evil-goggles--on) (evil-goggles--show-p ,beg ,end))
       (let ((evil-goggles--on t))
         ,@body2)
     ,body1))

(defmacro evil-goggles--with-no-hint (&rest body)
  "Do BODY with hints disabled."
  (declare (indent 0) (debug t))
  `(let ((evil-goggles--on t))
     ,@body))

(defmacro evil-goggles--with-pre-hint (beg end face dur &rest body)
  "Show hint from BEG to END with face FACE for DUR sec, then do BODY.

BODY is executed after the hint has been removed."
  (declare (indent 4) (debug t))
  `(evil-goggles--if-hint-on ,beg ,end (progn ,@body)
     (if (eq evil-this-type 'block)
         (evil-goggles--show-block-overlay ,beg ,end ,face (or ,dur evil-goggles-duration))
       (evil-goggles--show-overlay ,beg ,end ,face (or ,dur evil-goggles-duration)))
     ,@body))

(defmacro evil-goggles--show-overlay (beg end face dur &rest body)
  "Show overlay from BEG to END with face FACE for DUR seconds.

If BODY is non-nil, run BODY before removing the overlay.  The overlay
will be adjusted if BODY modifies the text in it."
  (declare (indent 4) (debug t))
  `(let ((ov (evil-goggles--make-overlay ,beg ,end 'insert-behind-hooks '(evil-goggles--overlay-insert-behind-hook))))
    (unwind-protect
        (progn
          (evil-goggles--show-or-pulse-overlay ov ,face ,dur)
          ,@body
          (sit-for ,dur))
      (delete-overlay ov))))

(defun evil-goggles--show-hint (beg end face dur)
  "Show hint from BEG to END with face FACE for DUR sec."
  ;; call evil-goggles--with-post-hint with no BODY
  (evil-goggles--with-post-hint beg end face dur))

(defun evil-goggles--show-block-overlay (beg end face dur)
  "Show overlay from BEG to END with face FACE for DUR seconds.

Pulsing the overlay isn't supported.
Running code while the hint is on isn't supported."
  ;; NOTE both of the limitation stated above can likely be addressed
  ;; if needed
  (let ((ovs)
        (overlay-face `(:background ,(evil-goggles--face-background face)))) ;; TODO drop this var
    (unwind-protect
        (progn
          ;; create multiple overlays, one for each line in the block
          (evil-apply-on-block (lambda (line-beg line-end)
                                 (add-to-list 'ovs
                                              (evil-goggles--make-overlay line-beg line-end 'face overlay-face)))
                               beg end nil)
          ;; TODO add support for pulsing a vertical block
          ;; (dolist (ov ovs) (evil-goggles--show-or-pulse-overlay ov face dur))
          (sit-for dur))
      (mapcar 'delete-overlay ovs))))

(defun evil-goggles--funcall-interactively (f &rest args)
  "Call F with ARGS interactively.

This function mimics `funcall-interactively', available in Emacs 25,
so this package can work with Emacs 24"
  (cl-letf (((symbol-function 'called-interactively-p) (lambda (_) t)))
    (apply f args)))

(defmacro evil-goggles--funcall-preserve-interactive (fun &rest args)
  "Call FUN with ARGS with `funcall' or `funcall-interactively'."
  `(if (called-interactively-p 'any)
       (evil-goggles--funcall-interactively ,fun ,@args)
     (funcall ,fun ,@args)))

(defmacro evil-goggles--define-switch-face-duration (switch-name switch-doc face-name face-doc dur-name dur-doc)
  "Helper macro defining an on/off var, a face, and duration var.

SWITCH-NAME is the name of the on/off variable.
SWITCH-DOC is the docstring for SWITCH-NAME.
FACE-NAME is the name of the custom face.
FACE-DOC is the docstring for FACE-NAME.
DUR-NAME is the name of the duration variable.
DUR-DOC is the docstring for DUR-NAME."
  (declare (indent 7) (debug t))
  `(progn
     (defcustom ,switch-name t
       ,(concat switch-doc "\nThis variable must be set before `evil-goggles-mode' is enabled")
       :type 'boolean
       :group 'evil-goggles)
     (defface ,face-name
       '((t (:inherit evil-goggles-default-face)))
       ,face-doc
       :group 'evil-goggles-faces)
     (defcustom ,dur-name nil
       ,(concat dur-doc "\nThis variable must be set before `evil-goggles-mode' is enabled")
       :type 'number
       :group 'evil-goggles)))

;;; core ends here ;;;

;; helper function to inherit from diff-mode's faces

(defun evil-goggles-use-diff-faces ()
  "Load `diff-mode' and use its faces for evil-goggles mode."
  (unless (require 'diff-mode nil 'no-error)
    (user-error "Can't load package diff-mode"))
  (custom-set-faces
   '(evil-goggles-delete-face           ((t (:inherit diff-removed))))
   '(evil-goggles-paste-face            ((t (:inherit diff-added))))
   '(evil-goggles-yank-face             ((t (:inherit diff-changed))))
   '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
   '(evil-goggles-undo-redo-add-face    ((t (:inherit diff-added))))
   '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))))

(defun evil-goggles-use-magit-faces ()
  "Load `magit-diff' and use its faces for evil-goggles mode."
  (unless (require 'magit-diff nil 'no-error)
    (user-error "Can't load package magit-diff, is magit installed?"))
  (custom-set-faces
   '(evil-goggles-delete-face           ((t (:inherit magit-diff-removed))))
   '(evil-goggles-paste-face            ((t (:inherit magit-diff-added))))
   '(evil-goggles-undo-redo-remove-face ((t (:inherit magit-diff-removed))))
   '(evil-goggles-undo-redo-add-face    ((t (:inherit magit-diff-added))))))

;; delete

(evil-goggles--define-switch-face-duration
    evil-goggles-enable-delete "If non-nil, enable delete support"
    evil-goggles-delete-face "Face for delete action"
    evil-goggles-delete-duration "Duration of hint when deleting")

(defun evil-goggles--evil-delete-advice (orig-fun beg end &optional type register yank-handler)
  "Around-advice for function `evil-delete`.

ORIG-FUN is the original function.
BEG END &OPTIONAL TYPE REGISTER YANK-HANDLER are the arguments of the original function."
  (evil-goggles--with-pre-hint beg end 'evil-goggles-delete-face evil-goggles-delete-duration
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type register yank-handler)))

;; indent

(evil-goggles--define-switch-face-duration
    evil-goggles-enable-indent "If non-nil, enable indent support"
    evil-goggles-indent-face "Face for indent action"
    evil-goggles-indent-duration "Duration of hint when indenting")

(defun evil-goggles--evil-indent-advice (orig-fun beg end)
  "Around-advice for function `evil-indent'.

ORIG-FUN is the original function.
BEG END are the arguments of the original function."
  (evil-goggles--with-post-hint beg end 'evil-goggles-indent-face evil-goggles-indent-duration
    (evil-goggles--funcall-preserve-interactive orig-fun beg end)))

;; yank

(evil-goggles--define-switch-face-duration
    evil-goggles-enable-yank "If non-nil, enable yank support"
    evil-goggles-yank-face "Face for yank action"
    evil-goggles-yank-duration "Duration of hint when yankig")

(defun evil-goggles--evil-yank-advice (orig-fun beg end &optional type register yank-handler)
  "Around-advice for function `evil-yank'.

ORIG-FUN is the original function.
BEG END &OPTIONAL TYPE REGISTER YANK-HANDLER are the arguments of the original function."
  (evil-goggles--with-post-hint beg end 'evil-goggles-yank-face evil-goggles-yank-duration
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type register yank-handler)))

;; undo & redo

(defcustom evil-goggles-enable-undo t
  "If non-nil, enable undo support.
This variable must be set before `evil-goggles-mode' is enabled"
  :type 'boolean :group 'evil-goggles)

(defcustom evil-goggles-enable-redo t
  "If non-nil, enable redo support.
This variable must be set before `evil-goggles-mode' is enabled"
  :type 'boolean :group 'evil-goggles)

(defcustom evil-goggles-undo-redo-add-duration evil-goggles-duration
  "Duration of hint on undo/redo adding.
This variable must be set before `evil-goggles-mode' is enabled"
  :type 'number :group 'evil-goggles)

(defcustom evil-goggles-undo-redo-remove-duration evil-goggles-duration
  "Duration of hint on undo/redo removing.
This variable must be set before `evil-goggles-mode' is enabled"
  :type 'number :group 'evil-goggles)

(defcustom evil-goggles-undo-redo-change-duration evil-goggles-duration
  "Duration of hint on undo/redo changing.
This variable must be set before `evil-goggles-mode' is enabled"
  :type 'number :group 'evil-goggles)

(defface evil-goggles-undo-redo-add-face
  '((t
     (:inherit evil-goggles-default-face)))
  "Face for undo/redo add action" :group 'evil-goggles-faces)

(defface evil-goggles-undo-redo-remove-face
  '((t
     (:inherit evil-goggles-default-face)))
  "Face for undo/redo remove action" :group 'evil-goggles-faces)

(defface evil-goggles-undo-redo-change-face
  '((t
     (:inherit evil-goggles-default-face)))
  "Face for undo/redo change action" :group 'evil-goggles-faces)

(defun evil-goggles--undo-tree-undo-advice (orig-fun &optional arg)
  "Advice for function `undo-tree-undo` and function `undo-tree-redo`.

ORIG-FUN is the original function.
ARG is the arguments of the original function."
  (unwind-protect
      (progn
        (advice-add 'primitive-undo :around 'evil-goggles--primitive-undo-advice)
        (funcall orig-fun arg))
    (advice-remove 'primitive-undo 'evil-goggles--primitive-undo-advice)))

(defun evil-goggles--primitive-undo-advice (orig-fun n list)
  "Advice for function `primitive-undo`.

ORIG-FUN is the original function.
N and LIST are the arguments of the original function."
  (let ((undo-item (evil-goggles--get-undo-item list)))
    ;; show hint on the text which will be removed before undo/redo removes it
    (pcase undo-item
      (`(text-added ,beg ,end)
       (evil-goggles--with-pre-hint beg end 'evil-goggles-undo-redo-remove-face evil-goggles-undo-redo-remove-duration)))

    ;; call the undo/redo function
    (funcall orig-fun n list)

    ;; show hint on the text which will be added after undo/redo addes it
    (pcase undo-item
      (`(text-removed ,beg ,end)
       (evil-goggles--with-pre-hint beg end 'evil-goggles-undo-redo-add-face evil-goggles-undo-redo-add-duration))
      (`(text-changed ,beg ,end)
       (evil-goggles--with-pre-hint beg end 'evil-goggles-undo-redo-change-face evil-goggles-undo-redo-change-duration)))))

(defun evil-goggles--get-undo-item (list)
  "Process LIST.

The LIST is the input variable to function `primitive-undo'.

This function tries to return a single list, either:
 ('text-added beg end), or:
 ('text-removed beg end)"
  (let* ((processed-list
          (evil-goggles--combine-undo-list (cl-remove-if #'null (mapcar #'evil-goggles--undo-elt list)))))
    ;; if there's only item in the list, return it; otherwise - nil
    (when (eq 1 (length processed-list))
      (car processed-list))))

(defun evil-goggles--combine-undo-list (input)
  "Combine elements in INPUT list.

Each element is expected to be either '(text-added BEG END) or
'(text-removed BEG END)."
  (let* ((last (car input))
         (result (list last)))
    (dolist (this (cdr input) (nreverse result))
      (cond ((and (eq (car last) 'text-added)
                  (eq (car last) (car this))
                  (eq (nth 1 last) (nth 1 this)))
             ;; combine 2 overlapping 'text-added elements
             (setcar result (list
                             (car this)
                             (nth 1 this)
                             (+ (nth 2 last) (abs (- (nth 1 this) (nth 2 this)))))))
            ((and (eq (car last) (car this))
                  (or
                   (eq (nth 1 last) (nth 2 this))
                   (eq (nth 2 last) (nth 1 this))))
             ;; combine 2 connecting text-added/text-deleted elements
             (setcar result (list
                             (car this)
                             (min (nth 1 this) (nth 2 this) (nth 1 last) (nth 2 last))
                             (max (nth 1 this) (nth 2 this) (nth 1 last) (nth 2 last)))))
            ((and
              (eq (car last) 'text-added)
              (eq (car this) 'text-removed)
              (eq (nth 1 last) (nth 1 this)))
             ;; combine overlapping text-added with text-removed which start at the same point
             (setcar result (list
                             'text-changed
                             (nth 1 last)
                             (if (< (nth 2 last) (nth 2 this))
                                 (max (nth 2 last) (nth 2 this))
                               (min (nth 2 last) (nth 2 this))))))
            (t (push this result)))
      (setq last (car result)))))

(defun evil-goggles--undo-elt (undo-elt)
  "Process UNDO-ELT.

Return a list: either ('text-added beg end) or ('text-removed beg end)"
  (pcase undo-elt
    ;; (BEG . END) means text added
    (`(,(and beg (pred integerp)) . ,(and end (pred integerp)))
     `(text-added ,beg ,end))
    ;; (TEXT . POSITION) means text inserted
    (`(,(and text (pred stringp)) . ,(and pos (pred integerp)))
     (list 'text-removed pos (+ pos (length text))))
    ;; All others return nil
    (_ nil)))

;; join

(evil-goggles--define-switch-face-duration
    evil-goggles-enable-join "If non-nil, enable join support"
    evil-goggles-join-face "Face for join action"
    evil-goggles-join-duration "Duration of hint when joining")

(defun evil-goggles--evil-join-advice (orig-fun beg end)
  "Around-advice for function `evil-join'.

ORIG-FUN is the original function.
BEG END are the arguments of the original function."
  (let* ((beg-line (line-number-at-pos beg))
         (end-line (line-number-at-pos end))
         (line-count (- end-line beg-line)))
    (if (> line-count 1) ;; don't show goggles for single lines ("J"/"gJ" without count)
        (evil-goggles--with-pre-hint beg end 'evil-goggles-join-face evil-goggles-join-duration
          (evil-goggles--funcall-preserve-interactive orig-fun beg end))
      (evil-goggles--funcall-preserve-interactive orig-fun beg end))))

;; reformat (fill and move)

(evil-goggles--define-switch-face-duration
    evil-goggles-enable-fill-and-move "If non-nil, enable fill and move (reformat) support"
    evil-goggles-fill-and-move-face "Face for fill and move (reformat) action"
    evil-goggles-fill-and-move-duration "Duration of hint when reformating")

(defun evil-goggles--evil-fill-and-move-advice (orig-fun beg end)
  "Around-advice for function `evil-fill-and-move'.

ORIG-FUN is the original function.
BEG END are arguments of the original function."
  (evil-goggles--with-post-hint beg end 'evil-goggles-fill-and-move-face evil-goggles-fill-and-move-duration
    (evil-goggles--funcall-preserve-interactive orig-fun beg end)))

;; paste before and after

(evil-goggles--define-switch-face-duration
    evil-goggles-enable-paste "If non-nil, enable paste support"
    evil-goggles-paste-face "Face for paste action"
    evil-goggles-paste-duration "Duration of hint when pasting")

(defun evil-goggles--evil-paste-after-advice (orig-fun count &optional register yank-handler)
  "Around-advice for function `evil-paste-after'.

ORIG-FUN is the original function.
COUNT REGISTER YANK-HANDLER are the arguments of the original function."
  (let ((was-in-normal-state (evil-normal-state-p))
        (orig-fun-result (evil-goggles--funcall-preserve-interactive orig-fun count register yank-handler)))
    (when was-in-normal-state
      (evil-goggles--evil-paste-show register yank-handler))
    orig-fun-result))

(defun evil-goggles--evil-paste-before-advice (orig-fun count &optional register yank-handler)
  "Around-advice for function `evil-paste-before'.

ORIG-FUN is the original function.
COUNT REGISTER YANK-HANDLER are the arguments of the original function."
  (let ((was-in-normal-state (evil-normal-state-p))
        (orig-fun-result (evil-goggles--funcall-preserve-interactive orig-fun count register yank-handler)))
    (when was-in-normal-state
      (evil-goggles--evil-paste-show register yank-handler))
    orig-fun-result))

(defun evil-goggles--evil-paste-show (register yank-handler)
  "Helper fun to show the goggles overlay on the last pasted text.

The overlay region is derermined by evil's marks [ and ]
Argument REGISTER is the evil register.
Argument YANK-HANDLER is the yank hanler."
  (unless evil-goggles--on
    (let* ((beg (save-excursion (evil-goto-mark ?\[) (point)))
           (end (save-excursion (evil-goto-mark ?\]) (point)))
           (is-beg-at-eol (save-excursion (goto-char beg) (eolp)))
           (beg-corrected (if is-beg-at-eol (1+ beg) beg)))
      (if (evil-goggles--evil-paste-block-p register yank-handler)
          ;; TODO evil-goggles--show-block-overlay doesn't check if the --on var is t/nil
          (evil-goggles--show-block-overlay beg-corrected end 'evil-goggles-paste-face evil-goggles-paste-duration)
        (evil-goggles--show-hint beg-corrected end 'evil-goggles-paste-face evil-goggles-paste-duration)))))

(defun evil-goggles--evil-paste-block-p (register yank-handler)
  "Return t if the paste was a vertical block.

Argument REGISTER is the evil register.
Argument YANK-HANDLER is the yank hanler."
  (let* ((text (if register
                   (evil-get-register register)
                 (current-kill 0)))
         (yh (or yank-handler
                 (when (stringp text)
                   (car-safe (get-text-property
                              0 'yank-handler text))))))
    (eq yh 'evil-yank-block-handler)))

;; shift left & right

(evil-goggles--define-switch-face-duration
    evil-goggles-enable-shift "If non-nil, enable shift left/right support"
    evil-goggles-shift-face "Face for paste action"
    evil-goggles-shift-duration "Duration of hint when shifting")

(defun evil-goggles--evil-shift-advice (orig-fun beg end &optional count preserve-empty)
  "Around-advice for function `evil-shift-left` and `evil-shift-right`.

ORIG-FUN is the original function.
BEG END &OPTIONAL COUNT PRESERVE-EMPTY are the arguments of the original function."
  (evil-goggles--with-post-hint beg end 'evil-goggles-shift-face evil-goggles-shift-duration
    (evil-goggles--funcall-preserve-interactive orig-fun beg end count preserve-empty)))

;; set mark

(evil-goggles--define-switch-face-duration
    evil-goggles-enable-set-marker "If non-nil, enable set mark support"
    evil-goggles-set-marker-face "Face for set mark action"
    evil-goggles-set-marker-duration "Duration of hint when setting mark")

(defun evil-goggles--evil-set-marker-advice (orig-fun char &optional pos advance)
  "Around-advice for function `evil-set-marker`.

ORIG-FUN is the original function.
CHAR POS ADVANCE are the arguments of the original function."
  ;; call orig-fun
  (evil-goggles--funcall-preserve-interactive orig-fun char pos advance)
  ;; maybe show the goggles overlay
  (when (<= ?a char ?z)
    (save-excursion
      (when pos
        (goto-char pos))
      (let ((beg (save-excursion
                   (move-beginning-of-line nil)
                   (point)))
            (end (1+ (save-excursion
                       (move-end-of-line nil)
                       (point)))))
        (evil-goggles--show-hint beg end 'evil-goggles-set-marker-face evil-goggles-set-marker-duration)))))

;; ex global

(defun evil-goggles--evil-ex-global-advice (orig-fun beg end pattern command &optional invert)
  "Around-advice for function `evil-ex-global'.

ORIG-FUN is the original function.
BEG END PATTERN COMMAND &OPTIONAL INVERT are the arguments of the original function."
  (evil-goggles--with-no-hint
    (evil-goggles--funcall-preserve-interactive orig-fun beg end pattern command invert)))

;; surround

(evil-goggles--define-switch-face-duration
    evil-goggles-enable-surround "If non-nil, enable surround support"
    evil-goggles-surround-face "Face for surround action"
    evil-goggles-surround-duration "Duration of hint when surrounding")

(defun evil-goggles--evil-surround-region-advice (orig-fun beg end &optional type char force-new-line)
  "Around-advice for function `evil-surround-region'.

ORIG-FUN is the original function.
BEG END &OPTIONAL TYPE CHAR FORCE-NEW-LINE are the arguments of the original function."
  (evil-goggles--with-pre-hint beg end 'evil-goggles-surround-face evil-goggles-surround-duration
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type char force-new-line)))

;; commentary

(evil-goggles--define-switch-face-duration
    evil-goggles-enable-commentary "If non-nil, enable commentary support"
    evil-goggles-commentary-face "Face for commentary action"
    evil-goggles-commentary-duration "Duration when using commentary")

(defun evil-goggles--evil-commentary-advice (orig-fun beg end &optional type)
  "Around-advice for function `evil-commentary'.

ORIG-FUN is the original function.
BEG END &OPTIONAL TYPE are the arguments of the original function."
  (evil-goggles--with-post-hint beg end 'evil-goggles-commentary-face evil-goggles-commentary-duration
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type)))

;; nerd-commenter

(evil-goggles--define-switch-face-duration
    evil-goggles-enable-nerd-commenter "If non-nil, enable nerd-commenter support"
    evil-goggles-nerd-commenter-face "Face for nerd-commenter action"
    evil-goggles-nerd-commenter-duration "Duration when using nerd-commenter")

(defun evil-goggles--evil-nerd-commenter-advice (orig-fun beg end &optional type)
  "Around-advice for function `evilnc-comment-operator'.

ORIG-FUN is the original function.
BEG END &OPTIONAL TYPE are the arguments of the original function."
  (evil-goggles--with-post-hint beg end 'evil-goggles-nerd-commenter-face evil-goggles-nerd-commenter-duration
    (evil-goggles--funcall-preserve-interactive orig-fun beg end type)))

;; replace with register

(evil-goggles--define-switch-face-duration
    evil-goggles-enable-replace-with-register "If non-nil, enable replace with register support"
    evil-goggles-replace-with-register-face "Face for replace with register action"
    evil-goggles-replace-with-register-duration "Duration when using replace-with-register")

(defun evil-goggles--evil-replace-with-register-advice (orig-fun count beg &optional end type register)
  "Around-advice for function `evil-replace-with-register'.

ORIG-FUN is the original function.
COUNT BEG &OPTIONAL END TYPE REGISTER are the arguments of the original function."
  (evil-goggles--with-post-hint beg end 'evil-goggles-nerd-commenter-face evil-goggles-replace-with-register-duration
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
  :require 'evil-goggles
  (cond
   (evil-goggles-mode

    ;; evil core functions

    (when evil-goggles-enable-delete
      (advice-add 'evil-delete :around 'evil-goggles--evil-delete-advice))

    (when evil-goggles-enable-indent
      (advice-add 'evil-indent :around 'evil-goggles--evil-indent-advice))

    (when evil-goggles-enable-yank
      (advice-add 'evil-yank :around 'evil-goggles--evil-yank-advice))

    (when evil-goggles-enable-undo
      (advice-add 'undo-tree-undo :around 'evil-goggles--undo-tree-undo-advice))
    (when evil-goggles-enable-redo
      (advice-add 'undo-tree-redo :around 'evil-goggles--undo-tree-undo-advice))

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

    (when evil-goggles-enable-set-marker
      (advice-add 'evil-set-marker :around 'evil-goggles--evil-set-marker-advice))

    ;; make sure :global and :v don't show the goggles overlay
    (advice-add 'evil-ex-global :around 'evil-goggles--evil-ex-global-advice)

    ;; evil non-core functions

    (when evil-goggles-enable-surround
      (advice-add 'evil-surround-region :around 'evil-goggles--evil-surround-region-advice))

    (when evil-goggles-enable-commentary
      (advice-add 'evil-commentary :around 'evil-goggles--evil-commentary-advice))

    (when evil-goggles-enable-nerd-commenter
      (advice-add 'evilnc-comment-operator :around 'evil-goggles--evil-nerd-commenter-advice))

    (when evil-goggles-enable-replace-with-register
      (advice-add 'evil-replace-with-register :around 'evil-goggles--evil-replace-with-register-advice)))
   (t
    (advice-remove 'evil-delete 'evil-goggles--evil-delete-advice)
    (advice-remove 'evil-indent 'evil-goggles--evil-indent-advice)
    (advice-remove 'evil-yank 'evil-goggles--evil-yank-advice)
    (advice-remove 'undo-tree-undo 'evil-goggles--undo-tree-undo-advice)
    (advice-remove 'undo-tree-redo 'evil-goggles--undo-tree-undo-advice)
    (advice-remove 'evil-join 'evil-goggles--evil-join-advice)
    (advice-remove 'evil-join-whitespace 'evil-goggles--evil-join-advice)
    (advice-remove 'evil-fill-and-move 'evil-goggles--evil-fill-and-move-advice)
    (advice-remove 'evil-paste-after 'evil-goggles--evil-paste-after-advice)
    (advice-remove 'evil-paste-before 'evil-goggles--evil-paste-before-advice)
    (advice-remove 'evil-shift-left 'evil-goggles--evil-shift-advice)
    (advice-remove 'evil-shift-right 'evil-goggles--evil-shift-advice)
    (advice-remove 'evil-set-marker 'evil-goggles--evil-set-marker-advice)

    (advice-remove 'evil-ex-global 'evil-goggles--evil-ex-global-advice)

    ;; evil non-core functions
    (advice-remove 'evil-surround-region 'evil-goggles--evil-surround-region-advice)
    (advice-remove 'evil-commentary 'evil-goggles--evil-commentary-advice)
    (advice-remove 'evilnc-comment-operator 'evil-goggles--evil-nerd-commenter-advice)
    (advice-remove 'evil-replace-with-register 'evil-goggles--evil-replace-with-register-advice))))

(provide 'evil-goggles)

;;; evil-goggles.el ends here
