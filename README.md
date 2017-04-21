[![Build Status](https://travis-ci.org/edkolev/evil-goggles.svg?branch=master)](https://travis-ci.org/edkolev/evil-goggles)

evil-goggles
=========

This package displays a visual hint when editing text in
`evil-mode`.

![change-bw-quotes](https://cloud.githubusercontent.com/assets/1532071/25258968/e140b104-264b-11e7-8097-ed40456698d6.gif)
![delete-line](https://cloud.githubusercontent.com/assets/1532071/25258971/e14c4a78-264b-11e7-8943-6b2197cf9a98.gif)
![disappear-on-input](https://cloud.githubusercontent.com/assets/1532071/25258970/e14bb46e-264b-11e7-814d-e64d8a26a308.gif)
![join-lines](https://cloud.githubusercontent.com/assets/1532071/25258972/e14d6412-264b-11e7-8d20-9c930c78c179.gif)
![maybe](https://cloud.githubusercontent.com/assets/1532071/25258973/e14e166e-264b-11e7-8ffb-7fafccc38324.gif)
![maybe2](https://cloud.githubusercontent.com/assets/1532071/25258969/e14a718a-264b-11e7-8d0e-221b84ecdeac.gif)
![paste-line](https://cloud.githubusercontent.com/assets/1532071/25258974/e156c200-264b-11e7-88ce-1316add482ca.gif)
![yank-line](https://cloud.githubusercontent.com/assets/1532071/25258975/e16109a4-264b-11e7-89da-b44dab56ffd9.gif)

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

Supported edit actions
----------------------

- delete
- yank
- paste with `p` and `P`
- indent (`=` operator)
- join
- format (`gq` operator)

- evil surround
- evil commentary
- evil replace with register

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
