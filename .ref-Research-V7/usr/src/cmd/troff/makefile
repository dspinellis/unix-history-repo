all:	nroff troff

nroff:
	make -f nmake
	rm *.o

troff:
	make -f tmake
	rm *.o

cp:	all
	cp nroff troff /bin
	rm nroff troff

cmp:	all
	cmp nroff /bin/nroff
	cmp troff /bin/troff
	rm nroff troff
