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
	raco setup --clean magnolisp

api-doc :
	-rm -r doc
	raco setup --no-zo --no-launcher --no-install --no-post-install magnolisp

pdf :
	-mkdir pdfs
	raco setup --no-zo --no-launcher --no-install --no-post-install --verbose --doc-pdf pdfs magnolisp

setup :
	raco setup magnolisp
