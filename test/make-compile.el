(setq files '("evil-goggles.el" "evil-goggles-faces.el"))
(setq byte-compile--use-old-handlers nil)
(mapc #'byte-compile-file files)


