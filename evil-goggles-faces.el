
;; load diff-mode faces
(require 'diff-mode)

;; evil core

(defface evil-goggles-delete-face
  '((t (:inherit diff-removed)))
  "Face for delete action"
  :group 'evil-goggles-faces)

(defface evil-goggles-indent-face
  '((t (:inherit region)))
  "Face for indent action"
  :group 'evil-goggles-faces)

(defface evil-goggles-yank-face
  '((t (:inherit region)))
  "Face for yank action"
  :group 'evil-goggles-faces)

(defface evil-goggles-join-face
  '((t (:inherit region)))
  "Face for join action"
  :group 'evil-goggles-faces)

(defface evil-goggles-fill-and-move-face
  '((t (:inherit region)))
  "Face for fill and move (reformat) action"
  :group 'evil-goggles-faces)

(defface evil-goggles-paste-face
  '((t (:inherit diff-added)))
  "Face for paste action"
  :group 'evil-goggles-faces)

;; non-core

(defface evil-goggles-surround-face
  '((t (:inherit region)))
  "Face for surround action"
  :group 'evil-goggles-faces)

(defface evil-goggles-commentary-face
  '((t (:inherit region)))
  "Face for commentary action"
  :group 'evil-goggles-sur)

(defface evil-goggles-replace-with-register-face
  '((t (:inherit region)))
  "Face for replace with register action"
  :group 'evil-goggles-sur)

(provide 'evil-goggles-faces)
