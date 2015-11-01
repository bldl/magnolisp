PKGNAME := magnolisp
VERSION :=
DISTSUFFIX := $(and $(VERSION),-$(VERSION))
DISTNAME := $(PKGNAME)$(DISTSUFFIX)
DISTHOME := $(PWD)/dist

default : setup

-include local.mk

# if `make setup` fails, you may need to do `make install` first
install :
	raco pkg install --name $(PKGNAME)
	$(MAKE) setup

uncrustify-install :
	-ln -s $(PWD)/etc/uncrustify.cfg $(HOME)/.uncrustify.cfg

uncrustify-sys-install :
	sudo aptitude install uncrustify

# 'raco setup' can also do this
custom-install-bin :
	-ln -s $(PWD)/mglc $(HOME)/bin/

clean :
	find -name compiled -type d -print0 | xargs -0 --no-run-if-empty rm -r

# locally avoid remote http://download.racket-lang.org/..../local-redirect/ links by regenerating manual with different settings
setup :
	raco setup $(PKGNAME)
	$(MAKE) api-doc

check-pkg-deps :
	raco setup --check-pkg-deps $(PKGNAME)

# takes a long time
clean-with-raco :
	raco setup --clean $(PKGNAME)

api-doc :
	mkdir -p doc/manual
	scribble ++xref-in setup/xref load-collections-xref --html --dest doc/manual --dest-name index.html manual-src/manual.scrbl

rm-dist :
	-rm -r $(DISTHOME)

pdf-manual :
	mkdir -p $(DISTHOME)
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --pdf --dest $(DISTHOME) --dest-name manual.pdf manual-src/manual.scrbl

html-manual :
	-rm -r $(DISTHOME)/manual
	mkdir -p $(DISTHOME)/manual
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --html --dest $(DISTHOME)/manual --dest-name index.html manual-src/manual.scrbl

MIRROR_DIR := /tmp/raco-tmp/magnolisp

# this indirection ensures that we only get what we would have in a Git repo
# (using 'git archive' would be more straightforward, but for future proofing)
pkg :
	-mkdir $(DISTHOME)
	-rm -r $(MIRROR_DIR)
	mkdir -p $(MIRROR_DIR)
	cp -ai ./ $(MIRROR_DIR)/
	( cd $(MIRROR_DIR) && git clean -dxff && rm -rf $(MIRROR_DIR)/.git && raco pkg create --format tgz --dest $(DISTHOME) --from-dir $(MIRROR_DIR) )

website-local :

website : rm-dist html-manual pdf-manual pkg website-local
	chmod -R a+rX $(DISTHOME)

test :
	raco test --jobs 2 --run-if-absent tests/run-*.rkt

test-with-cc :
	raco test tests/run-cc-on-files.rkt

gh-homepage :
	( cd gh-pages && git clean -d -f && git rm --ignore-unmatch -rf . )
	scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --html --dest gh-pages --dest-name gh-pages/index.html manual-src/manual.scrbl
	( cd gh-pages && git add . && git status )

gh-upload :
	( cd gh-pages && git commit -m "update $$(date)" && git push )
