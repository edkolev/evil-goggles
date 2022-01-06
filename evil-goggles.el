;;; evil-goggles.el --- Add a visual hint to evil operations -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2017 edkolev

;; Author: edkolev <evgenysw@gmail.com>
;; URL: http://github.com/edkolev/evil-goggles
;; Package-Requires: ((emacs "24.4") (evil "1.0.0"))
;; Version: 0.0.2
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
(require 'pulse)

(defcustom evil-goggles-duration 0.200
  "Time in floating seconds the goggles hint should last.

See also `evil-goggles-async-duration' and `evil-goggles-blocking-duration'."
  :type 'number
  :group 'evil-goggles)

(defcustom evil-goggles-async-duration nil
  "Time in floating seconds the async hint should last.

This affects the hints which are displayed after the operation has been
executed, highlighting the result of the operation.

If nil, the value of `evil-goggles-duration' will be used."
  :type 'number
  :group 'evil-goggles)

(defcustom evil-goggles-blocking-duration nil
  "Time in floating seconds the blocking hint should last.

This affects the blocking hints.  Such hints are displayed before the
operation, then the UI is blocked for the specified duration, then the
operation is executed.  Such an operation is delete, where the hint
only makes sense to be displayed before text is deleted.

If nil, the value of `evil-goggles-duration' will be used."
  :type 'number
  :group 'evil-goggles)

(defcustom evil-goggles-pulse #'display-graphic-p
  "Controls whether to pulse the hint or just appear and disapper.

If t, the hint will pulse always.
If nil, the hint will never pulse.
This variable can also hold a custom function which should return t or nil.

The default is to pulse if the display is graphical, otherwise not."
  :group 'evil-goggles
  :type '(choice
          (const :tag "Always" t)
          (const :tag "Never" nil)
          (function :tag "If graphical display" #'display-graphic-p)
          (function :tag "Custom function")))

(defface evil-goggles-default-face
  '((t (:inherit region)))
  "Evil-goggles default face."
  :group 'evil-goggles)

(defface evil-goggles--pulse-face nil
  "Temporary face used when pulsing, should not be customized.

This is needed because the pulse package expects to receive a face, it
can't work with input such as (backgound . \"red\")."
  :group 'evil-goggles)

(defun evil-goggles--pulse-p ()
  "Return whether to pulse or not, depending on variable `evil-goggles-pulse'."
  (if (functionp evil-goggles-pulse)
      (funcall evil-goggles-pulse)
    evil-goggles-pulse))

(defun evil-goggles--pulse-overlay (ov background dur)
  "Pulse the overlay OV with the BACKGROUND color for DUR duration.

This function returns immediately, it doesn't wait for the pulse
animation to end."
  (let* ((pulse-iterations 10)
         (pulse-delay (/ (float dur) pulse-iterations) ))
    (ignore pulse-delay pulse-iterations) ;; silence compile warnings for Unused lexical variable
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

(defun evil-goggles--show-p (beg end)
  "Return t if the overlay should be displayed in region BEG to END."
  (and (not evil-inhibit-operator-value)
       (bound-and-true-p evil-mode)
       (numberp beg)
       (numberp end)
       ;; don't show overlay if the region is a single char on a single line
       (not (and (<= (- end beg) 1)
                 (<= (count-lines beg end) 1)))
       (<= (point-min) beg end)
       (>= (point-max) end beg)
       (not (evil-visual-state-p))
       (not (evil-insert-state-p))
       ;; don't show overlay when evil-mc has active cursors
       (not (and (fboundp 'evil-mc-has-cursors-p) (evil-mc-has-cursors-p)))
       ;; don't show overlay when the region has only whitespace
       (not (null (string-match-p "[^ \t\n]" (buffer-substring-no-properties beg end))))))

(defun evil-goggles--overlay-insert-behind-hook (ov afterp beg end &optional len)
  "Function which grows/shriks the overlay OV when its text is changed.

The OV, AFTERP, BEG, END, LEN arguments are specified by the calling
convention for the insert-behind-hooks overlay property."
  (when afterp
    (if (zerop len)
        (progn
          (setq len (- end beg))
          (move-overlay ov (overlay-start ov) (+ len (overlay-end ov))))
      (move-overlay ov (overlay-start ov) (- (overlay-end ov) len) ))))

(defun evil-goggles--show-or-pulse-overlay (ov face dur)
  "Show or pulse overlay OV with face FACE.

DUR is used only when pulsing.
The decision to pulse or not is made by function
`evil-goggles--should-blink-or-pulse'."
  (pcase (evil-goggles--should-blink-or-pulse face)
    (`(do-blink ,blink-face)
     (overlay-put ov 'face blink-face))
    (`(do-pulse ,pulse-bg)
     (evil-goggles--pulse-overlay ov pulse-bg dur))))

(defun evil-goggles--should-blink-or-pulse (face)
  "Determine wheter to pulse or blink.

The decision is made based on the value of `evil-goggles-pulse'.

If the FACE has no background, pulsing is not supported, hence the
decision is to blink.  If the face has no foreground and/or background,
this function tries to make the most appropriate decision whether to
pulse or not, and whether to use the given FACE or use the fallback
face `evil-goggles-default-face'.

This function returns a list - either ('do-blink face) or ('do-pulse bg)."
  (let ((fg (face-foreground face nil t))
        (bg (face-background face nil t)))
    (cond
     ;; pulse enabled and the face has a bg - pulse with the given face's bg
     ((and (evil-goggles--pulse-p) bg)
      `(do-pulse ,bg))
     ;; pulse enabled and the face has no bg or fg - pulse with the default face's bg
     ((and (evil-goggles--pulse-p) (null bg) (null fg))
      `(do-pulse ,(face-background 'evil-goggles-default-face nil t)))
     ;; pulse disabled or face has fg only - show the hint with given face
     ((and (null bg) (null fg))
      `(do-blink evil-goggles-default-face))
     ;; else show the hint with the given face
     (t
      `(do-blink ,face)))))

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

(defun evil-goggles--show-block-overlay (beg end face dur)
  "Show overlay from BEG to END with face FACE for DUR seconds.

Pulsing the overlay isn't supported.
Running code while the hint is on isn't supported."
  ;; NOTE both of the limitation stated above can likely be addressed
  ;; if needed
  (let ((ovs))
    (unwind-protect
        (progn
          ;; create multiple overlays, one for each line in the block
          (evil-apply-on-block (lambda (line-beg line-end)
                                 (add-to-list 'ovs
                                              (evil-goggles--make-overlay line-beg line-end 'face face)))
                               beg end nil)
          ;; TODO add support for pulsing a vertical block
          ;; (dolist (ov ovs) (evil-goggles--show-or-pulse-overlay ov face dur))
          (sit-for dur))
      (mapcar 'delete-overlay ovs))))

;;; core ends here ;;;

;; helper function to inherit from diff-mode/magit-diff's faces

(defun evil-goggles-use-diff-faces ()
  "Use `diff-mode's diff-* faces for evil-goggles mode."
  (unless (require 'diff-mode nil 'no-error)
    (user-error "Can't load package diff-mode"))
  (custom-set-faces
   '(evil-goggles-delete-face           ((t (:inherit diff-removed))))
   '(evil-goggles-change-face           ((t (:inherit diff-removed))))
   '(evil-goggles-paste-face            ((t (:inherit diff-added))))
   '(evil-goggles-yank-face             ((t (:inherit diff-changed))))
   '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
   '(evil-goggles-undo-redo-add-face    ((t (:inherit diff-added))))
   '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))))

(defun evil-goggles-use-diff-refine-faces ()
  "Use `diff-mode's diff-refine-* faces for evil-goggles mode."
  (unless (require 'diff-mode nil 'no-error)
    (user-error "Can't load package diff-mode"))
  (custom-set-faces
   '(evil-goggles-delete-face           ((t (:inherit diff-refine-removed))))
   '(evil-goggles-change-face           ((t (:inherit diff-refine-removed))))
   '(evil-goggles-paste-face            ((t (:inherit diff-refine-added))))
   '(evil-goggles-yank-face             ((t (:inherit diff-refine-changed))))
   '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-refine-removed))))
   '(evil-goggles-undo-redo-add-face    ((t (:inherit diff-refine-added))))
   '(evil-goggles-undo-redo-change-face ((t (:inherit diff-refine-changed))))))

(defun evil-goggles-use-magit-faces ()
  "Load `magit-diff' and use its faces for evil-goggles mode."
  (unless (require 'magit-diff nil 'no-error)
    (user-error "Can't load package magit-diff, is magit installed?"))
  (custom-set-faces
   '(evil-goggles-delete-face           ((t (:inherit magit-diff-removed))))
   '(evil-goggles-paste-face            ((t (:inherit magit-diff-added))))
   '(evil-goggles-undo-redo-remove-face ((t (:inherit magit-diff-removed))))
   '(evil-goggles-undo-redo-add-face    ((t (:inherit magit-diff-added))))))

;;; generic blocking advice

(defun evil-goggles--show-blocking-hint (beg end &optional force-block)
  "Show blocking hint from BEG to END.

The hint will be a vertical block if `evil-this-type' is `block'.  If
FORCE-BLOCK is non-nil, the hint will always be a vertical block,
regardless of the value of `evil-this-type'."
  (let ((dur (or evil-goggles-blocking-duration evil-goggles-duration))
        (face (evil-goggles--get-face this-command)))
    (if (or (eq evil-this-type 'block) force-block)
        (evil-goggles--show-block-overlay beg end face dur)
      (evil-goggles--show-overlay beg end face dur))))

(defun evil-goggles--generic-blocking-advice (beg end &rest _)
  "Advice for interactive functions, show a blocing hint.

This function is intended to be used as advice for interactive funs
which take BEG and END as their first and second arguments."
  (when (and (called-interactively-p 'interactive)
             (evil-goggles--show-p beg end))
    (evil-goggles--show-blocking-hint beg end)))

;;; generic async advice

(defvar evil-goggles--timer nil)
(defvar evil-goggles--async-ov nil)

(defun evil-goggles--vanish (&rest _)
  "Remove the async overlay, cancel the timer, unregister from ‘pre-command-hook’."
  ;; user's C-g during this function execution should not result in
  ;; this function getting removed from pre-command-hook/run-at-time
  (with-local-quit
    (when (overlayp evil-goggles--async-ov)
      (delete-overlay evil-goggles--async-ov)
      (setq evil-goggles--async-ov nil))
    (when (timerp evil-goggles--timer)
      (cancel-timer evil-goggles--timer)
      (setq evil-goggles--timer nil))
    (remove-hook 'pre-command-hook 'evil-goggles--vanish)))

(defun evil-goggles--show-async-hint (beg end)
  "Show asynchronous hint from BEG to END."
  (let ((ov (evil-goggles--make-overlay beg end 'insert-behind-hooks '(evil-goggles--overlay-insert-behind-hook)))
        (dur (or evil-goggles-async-duration evil-goggles-duration))
        (face (evil-goggles--get-face this-command)))
    (unwind-protect
        ;; show the overlay
        (evil-goggles--show-or-pulse-overlay ov face dur)
      ;; any command by the user should prematurely cleanup the overlay
      (add-hook 'pre-command-hook #'evil-goggles--vanish)
      ;; remove the overlay with a timer
      (setq
       evil-goggles--async-ov ov
       evil-goggles--timer (run-at-time dur
                                        nil
                                        #'evil-goggles--vanish)))))

(defun evil-goggles--generic-async-advice (beg end &rest _)
  "Advice for interactive functions, show an async hint.

This function is intended to be used as advice for interactive funs
which take BEG and END as their first and second arguments."
  (when (and (called-interactively-p 'interactive)
             (evil-goggles--show-p beg end))
    (evil-goggles--show-async-hint beg end)))

(defun evil-goggles--generic-async-advice-1 (_ beg end &rest _rest)
  "Advice for interactive functions, show an async hint.

This function is intended to be used as advice for interactive funs
which take BEG and END as their second and third arguments."
  (when (and (called-interactively-p 'interactive)
             (evil-goggles--show-p beg end))
    (evil-goggles--show-async-hint beg end)))

;;; delete

(defcustom evil-goggles-enable-delete t
  "If non-nil, enable delete support.  Must be set before the mode is enabled."
  :type 'boolean :group 'evil-goggles)

(defface evil-goggles-delete-face '((t (:inherit evil-goggles-default-face)))
  "Face for delete action"
  :group 'evil-goggles-faces)

(defun evil-goggles--delete-line-advice (beg end &rest _)
  "Advice for `evil-delete-line'.

BEG and END are the argumenets to the original functions."
  (let ((beg (or beg (point)))
        (end (or end (line-end-position))))
    (when (and (called-interactively-p 'interactive)
               (evil-goggles--show-p beg end))
      (evil-goggles--show-blocking-hint beg end))))

;;; yank

(defcustom evil-goggles-enable-yank t
  "If non-nil, enable yank support.  Must be set before the mode is enabled."
  :type 'boolean :group 'evil-goggles)

(defface evil-goggles-yank-face '((t (:inherit evil-goggles-default-face)))
  "Face for yank action"
  :group 'evil-goggles-faces)

;;; change

(defcustom evil-goggles-enable-change t
  "If non-nil, enable change support.  Must be set before the mode is enabled."
  :type 'boolean :group 'evil-goggles)

(defface evil-goggles-change-face '((t (:inherit evil-goggles-default-face)))
  "Face for change action"
  :group 'evil-goggles-faces)

;;; indent

(defcustom evil-goggles-enable-indent t
  "If non-nil, enable indent support.  Must be set before the mode is enabled."
  :type 'boolean :group 'evil-goggles)

(defface evil-goggles-indent-face '((t (:inherit evil-goggles-default-face)))
  "Face for indent action"
  :group 'evil-goggles-faces)

;;; join

(defcustom evil-goggles-enable-join t
  "If non-nil, enable join support.  Must be set before the mode is enabled."
  :type 'boolean :group 'evil-goggles)

(defface evil-goggles-join-face '((t (:inherit evil-goggles-default-face)))
  "Face for join action"
  :group 'evil-goggles-faces)

(defun evil-goggles--join-advice (beg end &rest _)
  "Advice for `evil-join' and `evil-join-whitespace'.

BEG and END are the argumenets to the original functions."
  (when (and (called-interactively-p 'interactive)
             (evil-goggles--show-p beg end)
             ;; don't show goggles for single lines ("J"/"gJ" without count)
             (< 1 (count-lines beg end)))
    (evil-goggles--show-blocking-hint beg end)))

;;; fill

(defcustom evil-goggles-enable-fill-and-move t
  "If non-nil, enable fill-and-move support.  Must be set before the mode is enabled."
  :type 'boolean :group 'evil-goggles)

(defface evil-goggles-fill-and-move-face '((t (:inherit evil-goggles-default-face)))
  "Face for fill-and-move action"
  :group 'evil-goggles-faces)

;;; shift

(defcustom evil-goggles-enable-shift t
  "If non-nil, enable shift support.  Must be set before the mode is enabled."
  :type 'boolean :group 'evil-goggles)

(defface evil-goggles-shift-face '((t (:inherit evil-goggles-default-face)))
  "Face for shift action"
  :group 'evil-goggles-faces)

;;; evil-surround

(defcustom evil-goggles-enable-surround t
  "If non-nil, enable surround support.  Must be set before the mode is enabled."
  :type 'boolean :group 'evil-goggles)

(defface evil-goggles-surround-face '((t (:inherit evil-goggles-default-face)))
  "Face for surround action"
  :group 'evil-goggles-faces)

;;; evil-commentary

(defcustom evil-goggles-enable-commentary t
  "If non-nil, enable commentary support.  Must be set before the mode is enabled."
  :type 'boolean :group 'evil-goggles)

(defface evil-goggles-commentary-face '((t (:inherit evil-goggles-default-face)))
  "Face for commentary action"
  :group 'evil-goggles-faces)

;;; evil-nerd-commenter

(defcustom evil-goggles-enable-nerd-commenter t
  "If non-nil, enable nerd-commenter support.  Must be set before the mode is enabled."
  :type 'boolean :group 'evil-goggles)

(defface evil-goggles-nerd-commenter-face '((t (:inherit evil-goggles-default-face)))
  "Face for nerd-commenter action"
  :group 'evil-goggles-faces)

;;; evil-replace-with-register

(defcustom evil-goggles-enable-replace-with-register t
  "If non-nil, enable replace with register support.  Must be set before the mode is enabled."
  :type 'boolean :group 'evil-goggles)

(defface evil-goggles-replace-with-register-face '((t (:inherit evil-goggles-default-face)))
  "Face for replace with register action"
  :group 'evil-goggles-faces)

;;; set mark

(defcustom evil-goggles-enable-set-marker t
  "If non-nil, enable replace with register support.  Must be set before the mode is enabled."
  :type 'boolean :group 'evil-goggles)

(defface evil-goggles-set-marker-face '((t (:inherit evil-goggles-default-face)))
  "Face for replace with register action"
  :group 'evil-goggles-faces)

(defun evil-goggles--set-marker-advice (char &rest _)
  "Advice for `evil-set-marker'.

CHAR is an argument for the advice-d function."
  (when (and (called-interactively-p 'interactive)
             (<= ?a char ?z))
    (let ((beg (line-beginning-position))
          (end (1+ (line-end-position))))
      (evil-goggles--show-async-hint beg end))))

;;; record macro

(defcustom evil-goggles-enable-record-macro t
  "If non-nil, enable record macro support.  Must be set before the mode is enabled."
  :type 'boolean :group 'evil-goggles)

(defface evil-goggles-record-macro-face '((t (:inherit evil-goggles-default-face)))
  "Face for record macro action"
  :group 'evil-goggles-faces)

(defun evil-goggles--record-macro-advice (&rest _)
  "Advice for `evil-record-macro'."
  (let ((beg (line-beginning-position))
        (end (1+ (line-end-position)))
        (was-defining-kbd-macro defining-kbd-macro))

    ;; show hint before starting to record a macro
    (unless was-defining-kbd-macro
      (evil-goggles--show-async-hint beg end))

    ;; show hint when done defining the macro
    (when was-defining-kbd-macro
      (evil-goggles--show-async-hint beg end))))

;;; paste

(defcustom evil-goggles-enable-paste t
  "If non-nil, enable paste support.  Must be set before the mode is enabled."
  :type 'boolean :group 'evil-goggles)

(defface evil-goggles-paste-face '((t (:inherit evil-goggles-default-face)))
  "Face for paste action"
  :group 'evil-goggles-faces)

(defun evil-goggles--paste-advice (_ &optional register yank-handler)
  "Advice for `evil-paste-before' and `evil-paste-after'.

REGISTER and YANK-HANDLER are the argumenets to the original functions."
  (when (and (or
              ;; paste called interactively (via `p', `P')
              (called-interactively-p 'interactive)
              ;; paste called via evil-paste-pop/evil-paste-pop-next (via `C-n', `C-p')
              (memq real-this-command '(evil-paste-pop evil-paste-pop-next)))
             (evil-normal-state-p))
    (let* ((beg (save-excursion (evil-goto-mark ?\[) (if (eolp) (1+ (point)) (point))))
           (end (save-excursion (evil-goto-mark ?\]) (if (eolp) (1+ (point)) (point))))
           (is-vertical-block-pasted (evil-goggles--paste-vert-block-p register yank-handler)))
      (if is-vertical-block-pasted
          ;; XXX the async hint can't show vertical block hints - use a blocking hint if a vert block is pasted
          ;; XXX without the `(1+ end)', the vertical block hint is off by one
          (evil-goggles--show-blocking-hint beg (1+ end) is-vertical-block-pasted)
        (evil-goggles--show-async-hint beg end)))))


(defun evil-goggles--paste-vert-block-p (register yank-handler)
  "Return t if the paste is a vertical block.

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


;;; assosiation list with faces

(defvar evil-goggles--commands
  '((evil-delete                :face evil-goggles-delete-face                :switch evil-goggles-enable-delete                :advice evil-goggles--generic-blocking-advice)
    (evil-delete-line           :face evil-goggles-delete-face                :switch evil-goggles-enable-delete                :advice evil-goggles--delete-line-advice)
    (evil-org-delete            :face evil-goggles-delete-face                :switch evil-goggles-enable-delete                :advice evil-goggles--delete-line-advice)
    (evil-yank                  :face evil-goggles-yank-face                  :switch evil-goggles-enable-yank                  :advice evil-goggles--generic-async-advice)
    (evil-yank-line             :face evil-goggles-yank-face                  :switch evil-goggles-enable-yank                  :advice evil-goggles--generic-async-advice)
    (evil-change                :face evil-goggles-change-face                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
    (evil-change-line           :face evil-goggles-change-face                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
    (evil-change-whole-line     :face evil-goggles-change-face                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
    (evil-indent                :face evil-goggles-indent-face                :switch evil-goggles-enable-indent                :advice evil-goggles--generic-async-advice)
    (evil-join                  :face evil-goggles-join-face                  :switch evil-goggles-enable-join                  :advice evil-goggles--join-advice)
    (evil-join-whitespace       :face evil-goggles-join-face                  :switch evil-goggles-enable-join                  :advice evil-goggles--join-advice)
    (evil-fill-and-move         :face evil-goggles-fill-and-move-face         :switch evil-goggles-enable-fill-and-move         :advice evil-goggles--generic-async-advice)
    (evil-shift-left            :face evil-goggles-shift-face                 :switch evil-goggles-enable-shift                 :advice evil-goggles--generic-async-advice)
    (evil-shift-right           :face evil-goggles-shift-face                 :switch evil-goggles-enable-shift                 :advice evil-goggles--generic-async-advice)
    (evil-org-<                 :face evil-goggles-shift-face                 :switch evil-goggles-enable-shift                 :advice evil-goggles--generic-async-advice)
    (evil-org->                 :face evil-goggles-shift-face                 :switch evil-goggles-enable-shift                 :advice evil-goggles--generic-async-advice)
    (evil-surround-region       :face evil-goggles-surround-face              :switch evil-goggles-enable-surround              :advice evil-goggles--generic-async-advice)
    (evil-commentary            :face evil-goggles-commentary-face            :switch evil-goggles-enable-commentary            :advice evil-goggles--generic-async-advice)
    (evilnc-comment-operator    :face evil-goggles-nerd-commenter-face        :switch evil-goggles-enable-nerd-commenter        :advice evil-goggles--generic-async-advice)
    (evil-replace-with-register :face evil-goggles-replace-with-register-face :switch evil-goggles-enable-replace-with-register :advice evil-goggles--generic-async-advice-1)
    (evil-set-marker            :face evil-goggles-set-marker-face            :switch evil-goggles-enable-set-marker            :advice evil-goggles--set-marker-advice)
    (evil-record-macro          :face evil-goggles-record-macro-face          :switch evil-goggles-enable-record-macro          :advice evil-goggles--record-macro-advice)
    (evil-paste-before          :face evil-goggles-paste-face                 :switch evil-goggles-enable-paste                 :advice evil-goggles--paste-advice :after t)
    (evil-paste-after           :face evil-goggles-paste-face                 :switch evil-goggles-enable-paste                 :advice evil-goggles--paste-advice :after t)))

(defun evil-goggles--get-face (command)
  "Lookup face for COMMAND in `evil-goggles--commands'."
  (or
   (plist-get (cdr (assoc command evil-goggles--commands)) :face)
   'evil-goggles-default-face))

;;; minor mode defined below ;;;

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
  (if evil-goggles-mode
      (progn
        ;; add advice
        (dolist (command-cfg evil-goggles--commands)
          (let ((cmd (car command-cfg))
                 (advice (plist-get (cdr command-cfg) :advice))
                 (switch (plist-get (cdr command-cfg) :switch))
                 (after  (plist-get (cdr command-cfg) :after)))
            (when (symbol-value switch)
              (advice-add cmd (if after :after :before) advice)))))
    ;; remove advice
    (dolist (command-cfg evil-goggles--commands)
      (let ((cmd (car command-cfg))
             (advice (plist-get (cdr command-cfg) :advice)))
        (advice-remove cmd advice)))))

(provide 'evil-goggles)

;;; evil-goggles.el ends here
