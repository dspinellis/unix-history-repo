#	@(#)Makefile	5.3 (Berkeley) 5/14/90

CFLAGS=	-O -DLIBC_SCCS
OBJS=	crt0.o

all: ${OBJS}

crt0.o: crt0.c
	${CC} -S -DCRT0 ${.ALLSRC}
	grep -v "^#[ _]" < ${.PREFIX}.s > x.s
	ed x.s < :fix
	mv x.s ${.PREFIX}.s
	${CPP} -traditional crt0.s > x.s
	${AS} -o x.o x.s
	${LD} -x -r -o crt0.o x.o
	rm -f x.s x.o crt0.s

install:
	install -o ${BINOWN} -g ${BINGRP} -m 444 ${OBJS} \
	    ${DESTDIR}/usr/lib

cleandir clean:
	rm -f ${OBJS} gmon.o mon.o moncrt0.o core a.out x.s x.o crt0.s

depend lint tags:
