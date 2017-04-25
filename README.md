[![Build Status](https://travis-ci.org/edkolev/evil-goggles.svg?branch=master)](https://travis-ci.org/edkolev/evil-goggles)

evil-goggles
=========

`evil-goggles-mode` displays a visual hint when editing with [evil](https://github.com/emacs-evil/evil).


![yank-paste-delete](https://cloud.githubusercontent.com/assets/1532071/25314631/88a27184-2850-11e7-9272-7e1aaa3729bf.gif)


![change](https://cloud.githubusercontent.com/assets/1532071/25314980/2df8fbbc-2856-11e7-926f-8d23bcbda934.gif)


![join-lines](https://cloud.githubusercontent.com/assets/1532071/25258972/e14d6412-264b-11e7-8d20-9c930c78c179.gif)


![indent-region](https://cloud.githubusercontent.com/assets/1532071/25314629/889ae018-2850-11e7-9c9b-579edda38771.gif)


![fast-replace](https://cloud.githubusercontent.com/assets/1532071/25314628/889ab1c4-2850-11e7-9cf5-c801b8293583.gif)


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

## Actions with visual hint

#### edit actions with hint

- delete `evil-delete`
- yank `evil-yank`
- paste with `p` and `P` `evil-paste-before` and `evil-paste-after`
- indent (`=` operator) `evil-indent`
- join `evil-join` and `evil-join-whitespace`
- format (`gq` operator) `evil-fill-and-move`

#### evil plugins with hint

- [evil-surround](https://github.com/timcharper/evil-surround) `evil-surround-region`
- [evil-commentary](https://github.com/linktohack/evil-commentary) `evil-commentary`
- [evil-ReplaceWithRegister](https://github.com/Dewdrops/evil-ReplaceWithRegister) `evil-replace-with-register`

Customization
-------------

- The appearance of the overlay can be configured with `evil-goggles-default-face`. By default, the `region` face is used for every action. To get a list of available faces on emacs start, start a fresh emacs and run `M-x list-faces-display`.
```emacs-lisp
;; default is 'region, you can try `isearch-fail
(setq evil-goggles-default-face 'highlight)
```
- To use different faces per edit action:
```emacs-lisp
(require 'diff-mode) ;; load diff-* faces
(setq evil-goggles-faces-alist `(
                                 ( evil-delete . diff-removed )
                                 ( evil-yank . diff-changed )
                                 ( evil-paste-after . diff-added )
                                 ( evil-paste-before . diff-added )))
```

- The duration of the overlay is configured with `evil-goggles-duration`:
```emacs-lisp
(setq evil-goggles-duration 0.100) ;; default is 0.200 
```

- To disable the hint on certain actions, configure `evil-goggles-blacklist`
```emacs-lisp
;; to disable the hint when yanking or deleting
(setq evil-goggles-blacklist `(evil-yank evil-delete)
```
