what:
		@echo "what do you want to make?"

All:
		cd lib;			make All
		cd cmd;			make All

Dist-clean:
		cd lib;			make Dist-clean
		cd cmd;			make Dist-clean
		rm -f .BK* .CK* .emacs*
		rm -rf `gcomp Makefile lib cmd`
