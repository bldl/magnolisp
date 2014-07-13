PROJ_NAME := magnolisp

default : setup

-include local.mk

uncrustify-install :
	-ln -s $(PWD)/etc/uncrustify.cfg $(HOME)/.uncrustify.cfg

uncrustify-sys-install :
	sudo aptitude install uncrustify

# 'raco setup' can also do this
custom-install-bin :
	-ln -s $(PWD)/mglc $(HOME)/bin/

clean :
	find -name compiled -type d -print0 | xargs -0 --no-run-if-empty rm -r

# takes a long time
clean-with-raco :
	raco setup --clean magnolisp

bin :
	raco setup --no-zo --no-docs --no-foreign-libs --no-info-domain --no-pkg-deps magnolisp

bytecode :
	raco setup --no-launcher --no-docs --no-foreign-libs --no-info-domain --no-pkg-deps magnolisp

api-doc :
	-rm -r doc
	raco setup --no-zo --no-launcher --no-install --no-post-install magnolisp

DIST_HOME := $(PWD)/dist

rm-dist :
	-rm -r $(DIST_HOME)

# for this rule to produce .pdf, must do setup first, it seems
pdf :
	-mkdir $(DIST_HOME)
	raco setup --no-zo --no-launcher --no-install --no-post-install --verbose --doc-pdf $(DIST_HOME) magnolisp

html-doc :
	-rm -r $(DIST_HOME)/manual
	mkdir -p $(DIST_HOME)/manual
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --html --dest $(DIST_HOME)/manual --dest-name index.html manual.scrbl

MIRROR_DIR := /tmp/raco-tmp/magnolisp

# this indirection ensures that we only get what we would have in a Git repo
# (using 'git archive' would be more straightforward, but for future proofing)
pkg :
	-mkdir $(DIST_HOME)
	-rm -r $(MIRROR_DIR)
	mkdir -p $(MIRROR_DIR)
	cp -ai ./ $(MIRROR_DIR)/
	( cd $(MIRROR_DIR) && git clean -dxff && rm -rf $(MIRROR_DIR)/.git && raco pkg create --format tgz --dest $(DIST_HOME) --from-dir $(MIRROR_DIR) )

website-local :

website : rm-dist html-doc pdf pkg website-local
	chmod -R a+rX $(DIST_HOME)

setup :
	raco setup magnolisp

test :
	raco test --direct --run-if-absent tests/run-*.rkt
