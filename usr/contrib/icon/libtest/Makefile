what:
		@echo "what do you want to make?"

All:
		make Prog
		make Proc

Prog:
		cc -o sizes sizes.c
		Prog1test
		Prog2test

Proc:
		Proctest

Clean:
		rm -f local/*
		touch local/.placeholder

Dist-setup:
		make All
		cd ..;			make Pidemo
		rm -f distr/*
		mv local/* distr
		touch local/.placeholder

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm -f local/*
		touch local/.placeholder
		rm -rf `gcomp Makefile *.gen Prog* Proctest Functest t-* \
		   *.tlist local distr pdef.h sizes.c READ.ME`
