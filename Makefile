default : install

-include local.mk

install :
	-ln -s $(PWD)/mglcc $(HOME)/bin/
	-ln -s $(PWD)/etc/uncrustify.cfg $(HOME)/.uncrustify.cfg

sys-install :
	sudo aptitude install uncrustify
