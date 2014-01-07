default : install

-include local.mk

install :
	-ln -s $(PWD)/mglc $(HOME)/bin/
	-ln -s $(PWD)/etc/uncrustify.cfg $(HOME)/.uncrustify.cfg

sys-install :
	sudo aptitude install uncrustify

setup :
	raco setup --no-zo --no-launcher --no-install --no-post-install --verbose magnolisp

