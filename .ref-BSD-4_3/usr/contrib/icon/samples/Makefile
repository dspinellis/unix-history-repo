what:
		@echo "what do you want to make?"

All:
		make Std
		make Set
		make Xpx

Std:
		Test std

Xpx:
		Test xpx

Set:
		Test set

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
		rm -rf `gcomp Test.gen Makefile local distr *.icn *.dat *.tlist`
