CFLAGS = -n -O -s

cp:	all
	cp xsend xget enroll /bin
	rm *.o xsend xget enroll

cmp:	all
	cmp xsend /bin/xsend
	cmp xget /bin/xget
	cmp enroll /bin/enroll
	rm *.o xsend xget enroll

all:	enroll xsend xget
enroll:	enroll.o lib.o
	cc -n -s enroll.o lib.o -lmp -o enroll
xget:	xget.o lib.o
	cc -n -s xget.o lib.o -lmp -o xget
xsend:	xsend.o lib.o
	cc -n -s xsend.o lib.o -lmp -o xsend
enroll.o xget.o xsend.o lib.o:	xmail.h
list:
	@pr -2 -w124 m.h xmail.h enroll.c xsend.c xget.c lib.c makefile
