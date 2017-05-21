
;; load diff-mode faces
(require 'diff-mode)

(defface evil-goggles-delete-face
  '((t (:inherit diff-removed)))
  "Face for delete action"
  :group 'evil-goggles-faces)

(defface evil-goggles-indent-face
  '((t (:inherit region)))
  "Face for delete action"
  :group 'evil-goggles-faces)

(provide 'evil-goggles-faces)
