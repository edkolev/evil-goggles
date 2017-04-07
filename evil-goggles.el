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
;; next goals:
;; - ex commands: evil-copy, evil-move

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
       (numberp beg)
       (numberp end)
       (> (- end beg) 1)
       (<= (point-min) beg end)
       (>= (point-max) end beg)
       (not (evil-visual-state-p))
       (not (evil-insert-state-p))))

(defun evil-goggles--generic-advice (beg end orig-fun args face)
  (if (evil-goggles--show-p beg end)
      (let* ((evil-goggles--on t))
        (evil-goggles--show beg end face)
        (apply orig-fun args))
    (apply orig-fun args)))

(defun evil-goggles--evil-delete-advice (orig-fun beg end &optional type register yank-handler)
  (if (evil-goggles--show-p beg end)
      (let* ((evil-goggles--on t))
        (evil-goggles--show beg end (evil-goggles--face 'evil-delete))
        (funcall-interactively orig-fun beg end type register yank-handler))
    (funcall-interactively orig-fun beg end type register yank-handler)))

(defun evil-goggles--evil-indent-advice (orig-fun &rest args)
  (let ((beg (nth 0 args))
        (end (nth 1 args)))
    (evil-goggles--generic-advice beg end orig-fun args (evil-goggles--face 'evil-indent))))

(defun evil-goggles--evil-yank-advice (orig-fun &rest args)
  (let ((beg (nth 0 args))
        (end (nth 1 args)))
    (evil-goggles--generic-advice beg end orig-fun args (evil-goggles--face 'evil-yank))))

(defun evil-goggles--evil-join-advice (orig-fun &rest args)
  (let* ((beg (nth 0 args))
         (end (nth 1 args))
         (beg-line (line-number-at-pos beg))
         (end-line (line-number-at-pos end))
         (line-count (- end-line beg-line)))
    (if (> line-count 1) ;; don't show goggles for single lines ("J"/"gJ" without count)
        (evil-goggles--generic-advice beg end orig-fun args (evil-goggles--face 'evil-join))
      (apply orig-fun args))))

(defun evil-goggles--evil-surround-region (orig-fun &rest args)
  (let ((beg (nth 0 args))
        (end (nth 1 args)))
    (evil-goggles--generic-advice beg end orig-fun args (evil-goggles--face 'evil-surround))))

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
(evil-goggles--advice-add 'evil-delete          'evil-goggles--evil-delete-advice)
(evil-goggles--advice-add 'evil-indent          'evil-goggles--evil-indent-advice)
(evil-goggles--advice-add 'evil-yank            'evil-goggles--evil-yank-advice)
(evil-goggles--advice-add 'evil-join            'evil-goggles--evil-join-advice)
(evil-goggles--advice-add 'evil-join-whitespace 'evil-goggles--evil-join-advice)
(evil-goggles--advice-add 'evil-surround-region 'evil-goggles--evil-surround-region)

(provide 'evil-goggles)

;; evil-goggles.el end here
