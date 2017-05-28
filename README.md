[![Build Status](https://travis-ci.org/edkolev/evil-goggles.svg?branch=master)](https://travis-ci.org/edkolev/evil-goggles)

evil-goggles
=========

`evil-goggles-mode` displays a visual hint when editing with [evil](https://github.com/emacs-evil/evil).


![yank-paste-delete](https://cloud.githubusercontent.com/assets/1532071/25412512/ece4e108-29d7-11e7-90ba-834923c05a02.gif)


![change](https://cloud.githubusercontent.com/assets/1532071/25314980/2df8fbbc-2856-11e7-926f-8d23bcbda934.gif)


![join-lines](https://cloud.githubusercontent.com/assets/1532071/25258972/e14d6412-264b-11e7-8d20-9c930c78c179.gif)


![indent-region](https://cloud.githubusercontent.com/assets/1532071/25314629/889ae018-2850-11e7-9c9b-579edda38771.gif)


![fast-replace](https://cloud.githubusercontent.com/assets/1532071/25314628/889ab1c4-2850-11e7-9cf5-c801b8293583.gif)


Installation
------------

#### with [use-package](https://github.com/jwiegley/use-package) from [Melpa](https://melpa.org)
``` emacs-lisp
(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces so, for example,
  ;; deleted text will be highlighed with red color (as defined by the color theme)
  ;; (evil-goggles-use-diff-faces)
  )
```

#### without [use-package](https://github.com/jwiegley/use-package) from [Melpa](https://melpa.org)

`M-x package-install RET evil-goggles RET`, then add in `init.el`:

`(evil-goggles-mode)`

## Actions with visual hint

#### edit actions with hint

- delete
- yank
- paste
- indent (`=` operator)
- join
- format (`gq` operator)
- shift left and right (`>`, `<` operators)

#### evil plugins with hint

- surround [evil-surround](https://github.com/timcharper/evil-surround)
- commentary [evil-commentary](https://github.com/linktohack/evil-commentary)
- replace with register [evil-ReplaceWithRegister](https://github.com/Dewdrops/evil-ReplaceWithRegister)

Customization
-------------

- The appearance of the default overlay can be configured with `evil-goggles-default-face`. By default, the `region` face is used for every action. To get a list of available faces on emacs start, start a fresh emacs and run `M-x list-faces-display`.
```emacs-lisp
;; default is 'region, you can try 'isearch-fail
(custom-set-faces
 '(evil-goggles-default-face ((t (:inherit 'isearch-fail)))))
```
- To use different faces per edit action:
```emacs-lisp
(require 'diff-mode) ;; load diff-* faces
(custom-set-faces
 '(evil-goggles-delete-face ((t (:inherit 'diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit 'diff-added))))
 '(evil-goggles-yank-face ((t (:inherit 'diff-changed))))))
 
;; list of all evil-goggles-* faces:
;;
;; evil-goggles-delete-face
;; evil-goggles-indent-face
;; evil-goggles-yank-face
;; evil-goggles-join-face
;; evil-goggles-fill-and-move-face
;; evil-goggles-paste-face
;; evil-goggles-shift-face
;; evil-goggles-surround-face
;; evil-goggles-commentary-face
;; evil-goggles-replace-with-register-face
```

- The duration of the overlay is configured with `evil-goggles-duration`:
```emacs-lisp
(setq evil-goggles-duration 0.100) ;; default is 0.200
```

- To disable the hint on certain actions
```emacs-lisp
;; to disable the hint when yanking or deleting
(setq evil-goggles-enabe-paste nil)

;; list of all on/off variables:
;;
;; evil-goggles-enable-delete
;; evil-goggles-enable-indent
;; evil-goggles-enable-yank
;; evil-goggles-enable-join
;; evil-goggles-enable-fill-and-move
;; evil-goggles-enable-paste
;; evil-goggles-enable-shift
;; evil-goggles-enable-surround
;; evil-goggles-enable-commentary
;; evil-goggles-enable-replace-with-register
```
