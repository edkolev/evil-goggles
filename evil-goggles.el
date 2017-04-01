;; goals:
;; - delete with strikethrough preview
;; - yank with 'region' previw
;; - indent with 'underline' preview
;; next goals:
;; - one (advice) for all cases
;; - make the above configurable
;; next goals:
;; -  configurable integration with plugins: evil-surround, evil-lion
;; - '.' repeat with goggles

;; implementation:
;; - advise evil-delete, after
;; - if (and called interactively, not visual state, not insert state, goggles off, more than one char in region)
;;   - set goggles on
;;   - show goggles
;;   - call orig-fun
;;   - set gogges off
;; - else
;;   - call (interactively?) orig-fun

(defvar evil-goggles--on nil)
(defvar evil-goggles-show-for 0.200) ;; .100 or .200 seem best

(defun evil-goggles--show (beg end)
  (let ((ov (evil-goggles--make-overlay beg end 'face 'region)))
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
       (numberp beg)
       (numberp end)
       (> (- end beg) 1)
       (<= (point-min) beg end)
       (>= (point-max) end beg)
       (not (evil-visual-state-p))
       (not (evil-insert-state-p))))

(defun evil-goggles--evil-delete-advice (orig-fun &rest args)
  (let ((beg (nth 0 args))
        (end (nth 1 args)))
    (if (evil-goggles--show-p beg end)
        (let* ((evil-goggles--on t))
          (evil-goggles--show beg end)
          (apply orig-fun args))
      (apply orig-fun args))))

(define-minor-mode evil-goggles-mode
  "evil-goggles global minor mode."
  :lighter " (⌐■-■)"
  :global t
  (cond
   (evil-goggles-mode
    (advice-add 'evil-delete :around 'evil-goggles--evil-delete-advice))
   (t
    (advice-remove 'evil-delete 'evil-goggles--evil-delete-advice))))

(provide 'evil-goggles)

;; evil-goggles.el end here
