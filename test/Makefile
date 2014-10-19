EMACS=emacs

BATCH=$(EMACS) --batch -Q --eval '(require (quote org))'

%.el: %.org
	$(BATCH) --visit $^ --eval "(org-babel-tangle)"

all:
	bash -c 'for FILE in *.org; do make $${FILE%.org}.el; done'

