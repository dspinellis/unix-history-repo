#	Makefile	4.3	87/05/31
#
CFLAGS=	-O
DESTDIR=
ALL=	enroll xsend xget

all:	${ALL}

enroll:	enroll.o lib.o
	${CC} enroll.o lib.o -lmp -o enroll

xget:	xget.o lib.o
	${CC} xget.o lib.o -lmp -o xget

xsend:	xsend.o lib.o
	${CC} xsend.o lib.o -lmp -o xsend

enroll.o xget.o xsend.o lib.o:	xmail.h

list:
	@pr -2 -w124 m.h xmail.h enroll.c xsend.c xget.c lib.c Makefile

install: enroll xsend xget
	for i in ${ALL}; do install -s $$i ${DESTDIR}/usr/bin; done

clean:
	rm -f *.o yacc.* y.tab.c ${ALL}

depend:
