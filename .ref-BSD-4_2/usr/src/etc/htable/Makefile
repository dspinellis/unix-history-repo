# @(#)Makefile	4.6 11/3/83
CFLAGS=	-O
DESTDIR=
ALL=	htable

all:	${ALL}

htable:	htable.o parse.o scan.o
	${CC} htable.o parse.o scan.o -o htable

htable.o: htable.c htable.h
	${CC} ${CFLAGS} -c htable.c

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
