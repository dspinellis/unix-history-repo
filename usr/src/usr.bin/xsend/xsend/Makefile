#	Makefile	4.1	83/03/30
#
CFLAGS=	-O -d2
DESTDIR=

all:	enroll xsend xget

enroll:	enroll.o lib.o
	${CC} enroll.o lib.o -lmp -o enroll

xget:	xget.o lib.o
	${CC} xget.o lib.o -lmp -o xget

xsend:	xsend.o lib.o
	${CC} xsend.o lib.o -lmp -o xsend

enroll.o xget.o xsend.o lib.o:	xmail.h

list:
	@pr -2 -w124 m.h xmail.h enroll.c xsend.c xget.c lib.c makefile

install: enroll xsend xget
	install -s enroll ${DESTDIR}/usr/bin
	install -s xsend ${DESTDIR}/usr/bin
	install -s xget ${DESTDIR}/usr/bin

clean:
	rm -f *.o yacc.* y.tab.c
