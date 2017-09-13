
(let ((current-directory (file-name-directory load-file-name)))
  (setq evil-goggles-test-path (expand-file-name "." current-directory))
  (setq evil-goggles-root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path evil-goggles-root-path)
(add-to-list 'load-path evil-goggles-test-path)

(load (concat (file-name-as-directory evil-goggles-test-path) "evil-goggles-test.el") nil t)

(require 'cl)
(require 'ert)
(require 'evil)
(require 'evil-goggles)
(require 'evil-tests)

;; run evil's tests with evil-goggles-mode enabled
(evil-goggles-mode)
(evil-tests-initialize '() '())

