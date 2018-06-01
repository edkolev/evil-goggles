(setq package-user-dir
      (expand-file-name (format ".elpa/%s/elpa" emacs-version)))
(package-initialize)
(add-to-list 'load-path default-directory)
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
