CFLAGS = -O -d2

all:	enroll xsend xget

enroll:	enroll.o lib.o
	cc enroll.o lib.o -lmp -o enroll
xget:	xget.o lib.o
	cc xget.o lib.o -lmp -o xget
xsend:	xsend.o lib.o
	cc xsend.o lib.o -lmp -o xsend
enroll.o xget.o xsend.o lib.o:	xmail.h

list:
	@pr -2 -w124 m.h xmail.h enroll.c xsend.c xget.c lib.c makefile

install: enroll xsend xget
	install -s enroll $(DESTDIR)/usr/bin
	install -s xsend $(DESTDIR)/usr/bin
	install -s xget $(DESTDIR)/usr/bin

clean:
	rm -f *.o yacc.* y.tab.c
