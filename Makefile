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
	starter-kit-lisp.org			\
	starter-kit-misc-recommended.org	\
	starter-kit-org.org

EFILES = $(FILES:.org=.el)

TEST_FILES = test/emacs24-starter-kit-test.org test/install-ert-runner.org test/execute-ert-runner.org

all: $(EFILES)

bytes: $(EFILES:.el=.elc)

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

.PHONY : test clean TAGS

TAGS:
	find ~/.emacs.d -name '*.el' | etags -

test:
	$(BATCH) --eval '(org-babel-load-file "test/execute-ert-runner.org")'

# Packaging
NAME=literate-starter-kit
PACKAGE=$(NAME)

$(PACKAGE): $(wildcard *.org) init.el Makefile literate-starter-kit-pkg.el
	mkdir -p $(PACKAGE)
	cp $^ $(PACKAGE)
	$(BATCH) starter-kit.org --eval "(org-export-to-file 'ascii \"$(PACKAGE)/README\")"

$(PACKAGE).tar: $(PACKAGE)
	tar cf $@ $<

package: $(PACKAGE).tar

clean:
	for FILE in *.org; do EFILE=$${FILE%.org}.el; if [ -f $${EFILE} ]; then rm $${EFILE}; fi; done
	rm -f *.elc *.aux *.tex *.pdf starter-kit*.html doc/*html *~ .starter-kit*.part.org
	rm -rf $(PACKAGE).tar $(PACKAGE)
