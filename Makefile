emacs ?= emacs
bemacs = $(emacs) -batch -l test/elpa.el

update:
	$(emacs) -batch -l test/make-update.el

compile: clean
	$(bemacs) -l test/make-compile.el

test:
	$(bemacs) -l test/make-test.el

clean:
	rm -f *.elc test/evil-tests.el

checkdoc:
	$(bemacs) -l test/make-checkdoc.el

# wget evil-tests.el and comment out evil-test-command-window* and
# evil-test-jump tests which fail under emacs -batch;
# NOTE: the sed command doesn't work on BSD sed
test/evil-tests.el:
	curl -s "https://raw.githubusercontent.com/emacs-evil/evil/49965280b97d7ba8b913f4bf6ff86662e2263c4e/evil-tests.el" --output test/evil-tests.el
	sed -i '7471,7571{s/./;; &/}; 8080,8125{s/./;; &/}' test/evil-tests.el

evil-test: test/evil-tests.el
	$(emacs) -nw -Q -l test/elpa.el -l test/make-evil-test.el

evil-test-batch: test/evil-tests.el
	$(bemacs) -l test/make-evil-test.el

.PHONY: update compile test clean checkdoc evil-test wget-evil-tests evil-test-batch
