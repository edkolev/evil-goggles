emacs ?= emacs
bemacs = $(emacs) -batch -l test/elpa.el

all: compile

update:
	$(emacs) -batch -l test/make-update.el

compile: clean
	$(bemacs) -l test/make-compile.el

test:
	$(bemacs) -l test/make-test.el

clean:
	rm -f *.elc test/evil-tests.el test/evil-tests.el

checkdoc:
	$(bemacs) -l test/make-checkdoc.el

test/evil-tests.el:
	curl -s "https://raw.githubusercontent.com/emacs-evil/evil/master/evil-tests.el" --output test/evil-tests.el

evil-test: test/evil-tests.el
	$(emacs) -nw -Q -l test/elpa.el -l test/make-evil-test.el

evil-test-batch: test/evil-tests.el
	$(bemacs) -l test/make-evil-test.el

.PHONY: update compile test clean checkdoc evil-test wget-evil-tests evil-test-batch
