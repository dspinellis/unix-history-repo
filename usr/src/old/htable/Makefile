# @(#)Makefile	4.3 %G%
OPTIONS=-DLOCALNET=46
CFLAGS=	-O
DESTDIR=
ALL=	htable gettable

all:	${ALL}

htable:	htable.o parse.o scan.o
	${CC} htable.o parse.o scan.o -o htable

gettable: gettable.o
	${CC} gettable.o -o gettable

htable.o: htable.c htable.h
	${CC} ${CFLAGS} ${OPTIONS} -c htable.c

scan.o:	scan.c htable.h y.tab.h

parse.o: parse.c htable.h

scan.c:	scan.l

parse.c: parse.y

y.tab.h:
	${YACC} -d parse.y
	rm y.tab.c

clean:
	rm -f ${ALL} *.o *.s y.tab.h errs

install:
	install -s htable ${DESTDIR}/etc/htable
	install -s gettable ${DESTDIR}/etc/gettable
