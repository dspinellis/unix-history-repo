what:
		@echo "what do you want to make?"

All:
		Trantest
		Linktest
		Runtestall

Tran:
		Trantest

Link:
		Linktest

Arith:
		Runtest arith

Basis:
		Runtest basis

Display:
		Runtest display

Esusp:
		Runtest esusp

Fail:
		Runtest fail

Gc:
		Runtest gc

Lsusp:
		Runtest lsusp

Pret:
		Runtest pret

Psusp:
		Runtest psusp

Suspend:
		Runtest suspend

Linkcheck:
		-Linkchecker

Clean:
		rm -f local/*
		touch local/.placeholder

Dist-setup:
		make All
		mv local/* distr
		touch local/.placeholder

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm -f local/*
		touch local/.placeholder
		rm -rf `gcomp *.gen Makefile *.tlist Linkchecker \
		   Runtestall *.icn local distr`
