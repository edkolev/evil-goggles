[![Build Status](https://travis-ci.org/edkolev/evil-goggles.svg?branch=master)](https://travis-ci.org/edkolev/evil-goggles)
[![NonGNU ELPA](https://elpa.nongnu.org/nongnu/evil-goggles.svg)](https://elpa.nongnu.org/nongnu/evil-goggles.html)
[![MELPA](https://melpa.org/packages/evil-goggles-badge.svg)](https://melpa.org/#/evil-goggles)

evil-goggles
============

![evil-goggles](https://cloud.githubusercontent.com/assets/1532071/26526401/2d10961e-4382-11e7-8c40-5b7fb3a79756.jpg)

`evil-goggles-mode` displays a visual hint when editing with [evil](https://github.com/emacs-evil/evil).

## Preview

![yank-paste-delete](https://cloud.githubusercontent.com/assets/1532071/25412512/ece4e108-29d7-11e7-90ba-834923c05a02.gif)


![change](https://cloud.githubusercontent.com/assets/1532071/25314980/2df8fbbc-2856-11e7-926f-8d23bcbda934.gif)


![join-lines](https://cloud.githubusercontent.com/assets/1532071/25258972/e14d6412-264b-11e7-8d20-9c930c78c179.gif)


![indent-region](https://cloud.githubusercontent.com/assets/1532071/25314629/889ae018-2850-11e7-9c9b-579edda38771.gif)


![undo-redo](https://user-images.githubusercontent.com/1532071/30509413-ce64fc84-9ab8-11e7-9f33-107c1a62f653.gif)


![fast-replace](https://cloud.githubusercontent.com/assets/1532071/25314628/889ab1c4-2850-11e7-9cf5-c801b8293583.gif)

## Usage

Enable `(evil-goggles-mode)`, then edit text like you normally would,
try for example `yy`, `p`, `dd` in normal state.

## Installation

#### with [use-package](https://github.com/jwiegley/use-package) from [NonGNU ELPA](https://elpa.nongnu.org/) or [MELPA](https://melpa.org)
``` emacs-lisp
(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))
```

#### without [use-package](https://github.com/jwiegley/use-package) from [NonGNU ELPA](https://elpa.nongnu.org/) or [MELPA](https://melpa.org)

`M-x package-install RET evil-goggles RET`, then add in `init.el`:

`(evil-goggles-mode)`

## Actions with visual hint

#### edit actions with hint

- delete
- change
- yank
- paste
- indent (`=` operator)
- join
- format (`gq` operator)
- shift left and right (`>`, `<` operators)
- undo
- redo
- set mark
- start/stop macro recording

#### evil plugins with hint

- surround [evil-surround](https://github.com/timcharper/evil-surround)
- commentary [evil-commentary](https://github.com/linktohack/evil-commentary)
- nerd-commenter [evil-nerd-commenter](https://github.com/redguardtoo/evil-nerd-commenter)
- replace with register [evil-ReplaceWithRegister](https://github.com/Dewdrops/evil-ReplaceWithRegister)
- evil-org [evil-org](https://github.com/Somelauw/evil-org-mode/issues)

## Customizations

#### Appearance Customization

- To pulse the visual hint, rather than just show and hide it:

``` emacs-lisp
(setq evil-goggles-pulse t) ;; default is to pulse when running in a graphic display
```

- To change the default face:

```emacs-lisp
(custom-set-faces
 '(evil-goggles-default-face ((t (:inherit 'highlight))))) ;; default is to inherit 'region
;; run `M-x list-faces-display` in a fresh emacs to get a list of faces on your emacs
```

By default, all goggles' faces inherit `evil-goggles-default-face`, which in turn inherits emacs' `region` face.

- To use different faces per edit action:
```emacs-lisp
(custom-set-faces
 '(evil-goggles-delete-face ((t (:inherit 'shadow))))
 '(evil-goggles-paste-face ((t (:inherit 'lazy-highlight))))
 '(evil-goggles-yank-face ((t (:inherit 'isearch-fail)))))
```

- The following faces are defined by evil-goggles:
```
evil-goggles-default-face - inherits from `region` by default

evil-goggles-delete-face - this, and the others below, inherit from `evil-goggles-default-face`
evil-goggles-change-face
evil-goggles-indent-face
evil-goggles-yank-face
evil-goggles-join-face
evil-goggles-fill-and-move-face
evil-goggles-paste-face
evil-goggles-shift-face
evil-goggles-surround-face
evil-goggles-commentary-face
evil-goggles-nerd-commenter-face
evil-goggles-replace-with-register-face
evil-goggles-set-marker-face
evil-goggles-undo-redo-add-face
evil-goggles-undo-redo-remove-face
evil-goggles-undo-redo-change-face
evil-goggles-record-macro-face
```

#### Other Customizations

- The duration of the overlay is configured with `evil-goggles-duration`:
```emacs-lisp
(setq evil-goggles-duration 0.100) ;; default is 0.200
```

- For more fine grained duration configuration, these can be modified:

``` emacs-lisp
;; this variable affects "blocking" hints, for example when deleting - the hint is displayed,
;; the deletion is delayed (blocked) until the hint disappers, then the hint is removed and the
;; deletion executed; it makes sense to have this duration short
(setq evil-goggles-blocking-duration 0.100) ;; default is nil, i.e. use `evil-goggles-duration'

;; this variable affects "async" hints, for example when indenting - the indentation
;; is performed with the hint visible, i.e. the hint is displayed, the action (indent) is
;; executed (asynchronous), then the hint is removed, highlighting the result of the indentation
(setq evil-goggles-async-duration 0.900) ;; default is nil, i.e. use `evil-goggles-duration'
```

- To disable the hint on certain actions modify these variable before `evil-goggles-mode` is started:
```emacs-lisp
;; to disable the hint when pasting:
(setq evil-goggles-enable-paste nil)

;; list of all on/off variables, their default value is `t`:
;;
;; evil-goggles-enable-delete
;; evil-goggles-enable-change
;; evil-goggles-enable-indent
;; evil-goggles-enable-yank
;; evil-goggles-enable-join
;; evil-goggles-enable-fill-and-move
;; evil-goggles-enable-paste
;; evil-goggles-enable-shift
;; evil-goggles-enable-surround
;; evil-goggles-enable-commentary
;; evil-goggles-enable-nerd-commenter
;; evil-goggles-enable-replace-with-register
;; evil-goggles-enable-set-marker
;; evil-goggles-enable-undo
;; evil-goggles-enable-redo
;; evil-goggles-enable-record-macro
```

## NEWS - Recent Significant Changes

- [Jul 01, 2018] Make async hint cleanup more robust
- [Jun 01, 2018] Refactor code to not use :around advice-s, which was a source of edge-case-issues
- [Feb 05, 2018] Show hint on start/stop macro recording
- [Dec 02, 2017] Pulsing hints is no longer experimental
- [Nov 03, 2017] Add options `evil-goggles-async-duration` and `evil-goggles-blocking-duration`
- [Sep 17, 2017] Add experimental support for pulsing hints (no longer experimental since Dec 02, 2017)
- [Sep 16, 2017] Support for undo/redo is no longer experimental
- [Sep 13, 2017] Emacs 24 support
- [Aug 02, 2017] Add experimental support for undo/redo (no longer experimental since Sep 16, 2017)
- [May 28, 2017] Switched to using per-action on/off custom variables, deprecated `evil-goggles-blacklist`
- [May 28, 2017] Switched to using custom faces per action, deprecated `evil-goggles-faces-alist`
