#	@(#)Makefile	8.1 (Berkeley) 6/6/93

all: pcexterns.o

pcexterns.o: ${.CURDIR}/pcexterns.awk ${LIBPC} ${LIBC} ${LIBM}
	(echo -n ${LIBPC}:; nm -go ${LIBPC}) \
	    | awk -f ${.CURDIR}/pcexterns.awk \
	    | sort -t\" +1 -2 -u > pcexterns.s
	(echo -n ${LIBC}:; nm -go ${LIBC}) \
	    | awk -f ${.CURDIR}/pcexterns.awk \
	    | sort -t\" +1 -2 -u >> pcexterns.s
	(echo -n ${LIBM}:; nm -go ${LIBM}) \
	    | awk -f ${.CURDIR}/pcexterns.awk \
	    | sort -t\" +1 -2 -u >> pcexterns.s
	${AS} pcexterns.s -o ${.TARGET}
	rm -f pcexterns.s

clean cleandir:
	rm -f pcexterns.o pcexterns.s

depend lint:

install:
	install -o ${BINOWN} -g ${BINGRP} -m 444 pcexterns.o \
	    ${DESTDIR}/usr/lib

.include <bsd.prog.mk>
