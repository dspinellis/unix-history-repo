#	@(#)Makefile	5.3 (Berkeley) 6/8/90

OBJS=	getline.o getnum.o

all objects: ${OBJS}

clean cleandir:
	rm -f ${OBJS}

depend lint tags:

install:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/getline.c \
	    ${.CURDIR}/getnum.c ${DESTDIR}${LLIB}/learn/C
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${OBJS} \
	    ${DESTDIR}${LLIB}/learn/bin
	install -o ${BINOWN} -g ${BINGRP} -m 444 ${OBJS} \
	    ${DESTDIR}${LLIB}/learn/C

.include <bsd.prog.mk>
