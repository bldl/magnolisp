VERSION := 
PROJ_NAME := magnolisp$(and $(VERSION),-$(VERSION))

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

setup :
	raco setup magnolisp

# takes a long time
clean-with-raco :
	raco setup --clean magnolisp

bin :
	raco setup --no-zo --no-docs --no-foreign-libs --no-info-domain --no-pkg-deps magnolisp

bytecode :
	raco setup --no-launcher --no-docs --no-foreign-libs --no-info-domain --no-pkg-deps magnolisp

api-doc :
	-rm -r doc
	mkdir -p doc/manual
	scribble ++xref-in setup/xref load-collections-xref --html --dest doc/manual --dest-name index.html manual.scrbl

# creates a slightly different directory structure
api-doc-with-raco :
	-rm -r doc
	raco setup --no-zo --no-launcher --no-install --no-post-install magnolisp

DIST_HOME := $(PWD)/dist

rm-dist :
	-rm -r $(DIST_HOME)

pdf-manual :
	mkdir -p $(DIST_HOME)
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --pdf --dest $(DIST_HOME) --dest-name manual.pdf manual.scrbl

html-manual :
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

website : rm-dist html-manual pdf-manual pkg website-local
	chmod -R a+rX $(DIST_HOME)

test :
	raco test --direct --run-if-absent tests/run-*.rkt

gh-homepage :
	( cd gh-pages && git clean -d -f && git rm --ignore-unmatch -rf . )
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --html --dest gh-pages --dest-name gh-pages/index.html manual.scrbl
	( cd gh-pages && git add . && git status )

gh-upload :
	( cd gh-pages && git commit -m "update $$(date)" && git push )
