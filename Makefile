default : setup

-include local.mk

install :
	-ln -s $(PWD)/mglc $(HOME)/bin/
	-ln -s $(PWD)/etc/uncrustify.cfg $(HOME)/.uncrustify.cfg

sys-install :
	sudo aptitude install uncrustify

clean :
	raco setup --clean magnolisp

doc :
	raco setup --no-zo --no-launcher --no-install --no-post-install --verbose magnolisp

pdf :
	-mkdir pdfs
	raco setup --no-zo --no-launcher --no-install --no-post-install --verbose --doc-pdf pdfs magnolisp

setup :
	raco setup magnolisp
