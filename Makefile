# Name of your emacs binary
EMACS=emacs

BATCH=$(EMACS) --batch -Q --eval '(require (quote org))' --eval '(setq starter-kit-dir default-directory)'

FILES = starter-kit.org				\
	starter-kit-bindings.org		\
	starter-kit-defuns.org			\
	starter-kit-misc.org			\
	starter-kit-registers.org		\
	starter-kit-eshell.org			\
	starter-kit-nodejs.org			\
	starter-kit-semantic.org		\
	starter-kit-lisp.org			\
	starter-kit-misc-recommended.org	\
	starter-kit-org.org

EFILES = $(FILES:.org=.el)

TEST_FILES = test/emacs24-starter-kit-test.org test/install-ert-runner.org test/execute-ert-runner.org

all: $(EFILES)

%.el: %.org
	$(BATCH) --eval '(org-babel-load-file "$<")'

%.elc: %.el
	$(BATCH) -f batch-byte-compile "$<"

doc: doc/index.html

doc/index.html:
	mkdir -p doc
	$(EMACS) --batch -Q --eval '(org-babel-load-file "starter-kit-publish.org")'
	rm starter-kit-publish.el
	cp doc/starter-kit.html doc/index.html
	echo "Documentation published to doc/"

.PHONY : test clean

test:
	$(BATCH) --eval '(mapc (lambda (x) (org-babel-load-file (symbol-name x))) (quote ($(TEST_FILES))))'

clean:
	rm -f *.elc *.aux *.tex *.pdf starter-kit*.el starter-kit*.html doc/*html *~ .starter-kit*.part.org
	for FILE in *.org; do EFILE=$${FILE%.org}.el; if [ -f $${EFILE} ]; then rm $${EFILE}; fi; done
