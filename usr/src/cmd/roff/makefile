all:	roff

cp:	roff
	cp roff /bin
	rm roff roff.o

cmp:	roff
	cmp roff /bin/roff
	rm roff roff.o

roff:	*.s
	as -o roff.o /usr/include/sys.s *.s
	ld -s roff.o -lc -o roff
