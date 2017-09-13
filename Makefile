emacs ?= emacs
bemacs = $(emacs) -batch -l test/elpa.el

update:
	$(emacs) -batch -l test/make-update.el

compile: clean
	$(bemacs) -l test/make-compile.el

test:
	$(bemacs) -l test/make-test.el

clean:
	rm -f *.elc

checkdoc:
	$(bemacs) -l test/make-checkdoc.el

wget-evil-tests:
	curl "https://raw.githubusercontent.com/emacs-evil/evil/49965280b97d7ba8b913f4bf6ff86662e2263c4e/evil-tests.el" --output test/evil-tests.el

evil-test:
	$(emacs) -nw -Q -l test/elpa.el -l test/make-evil-test.el

.PHONY: update compile test clean checkdoc evil-test wget-evil-tests
