[![Build Status](https://travis-ci.org/edkolev/evil-goggles.svg?branch=master)](https://travis-ci.org/edkolev/evil-goggles)

evil-goggles
=========

This package displays a visual hint when editing text in
`evil-mode`.

For example `yy` will briefly highlight the line to
indicate it has just been yanked, `diw` will highlight the "inner word"
right before deleting it, `p` and `P` will highlight the newly pasted
text for a short moment.

Installation
------------

#### with [use-package](https://github.com/jwiegley/use-package)
``` emacs-lisp
;; NOTE: this doesn't work yet, package is not on Melpa
(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode))
```

#### without [use-package](https://github.com/jwiegley/use-package)

`M-x package-install RET evil-goggles RET`, then add in `init.el`:

`(evil-goggles-mode)`

Examples
--------

...


Customization
-------------

- `evil-goggles-show-for` 0.200
- `evil-goggles-default-face` 'region
- `evil-goggles-faces-alist` nil
``` emacs-lisp
  (setq evil-goggles-faces-alist (
    ( evil-delete . evil-ex-substitute-matches ) ;; isearch-fail
    ( evil-yank . evil-ex-substitute-replacement )
    ))
```
- `evil-goggles-blacklist` nil
