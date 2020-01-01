
(let ((current-directory (file-name-directory load-file-name)))
  (setq evil-goggles-test-path (expand-file-name "." current-directory)))

(add-to-list 'load-path evil-goggles-test-path) ;; so evil-tests can be loaded below

(require 'cl)
(require 'ert)
(require 'evil)
(require 'evil-goggles)
(require 'evil-tests)

;; run evil's tests with evil-goggles-mode enabled
(evil-goggles-mode)
(evil-tests-initialize '() '())

