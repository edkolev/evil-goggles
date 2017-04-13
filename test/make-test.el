
(let ((current-directory (file-name-directory load-file-name)))
  (setq evil-goggles-test-path (expand-file-name "." current-directory))
  (setq evil-goggles-root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path evil-goggles-root-path)
(add-to-list 'load-path evil-goggles-test-path)

(load (concat (file-name-as-directory evil-goggles-test-path) "evil-goggles-test.el") nil t)

(ert-run-tests-batch-and-exit)
