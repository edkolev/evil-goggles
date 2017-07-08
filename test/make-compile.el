(setq files '("evil-goggles.el"))
(setq byte-compile-error-on-warn t)
(setq byte-compile--use-old-handlers nil)
(mapc (lambda (file)
        (unless (byte-compile-file file)
          (kill-emacs 1)))
      files)


