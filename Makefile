default : setup

-include local.mk

install :
	-ln -s $(PWD)/etc/uncrustify.cfg $(HOME)/.uncrustify.cfg

sys-install :
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

api-doc :
	-rm -r doc
	raco setup --no-zo --no-launcher --no-install --no-post-install magnolisp

# for this rule to produce .pdf, must do setup first, it seems
pdf :
	-mkdir pdfs
	raco setup --no-zo --no-launcher --no-install --no-post-install --verbose --doc-pdf pdfs magnolisp

setup :
	raco setup magnolisp

test :
	raco test tests/run-*.rkt
