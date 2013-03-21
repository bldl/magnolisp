install :
	ln -s $(PWD)/etc/uncrustify.cfg $(HOME)/.uncrustify.cfg

sys-install :
	sudo aptitude install uncrustify
