what:
		@echo "what do you want to make?"

All:
		make Std
		make Set
		make Xpx

Std:
		Test std

Set:
		Test set

Xpx:
		Test xpx

Listall:	
		@pr *.icn
		@date >List

List:	
		@pr $?
		@date >List

Clean:
		rm -f local/*
		touch local/.placeholder

Dist-setup:	
		make All
		rm -f distr/*
		mv local/* distr
		touch local/.placeholder

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm -f local/*
		touch local/.placeholder
		rm -f `gcomp READ.ME Makefile Test.gen local distr *.icn *.tlist`
