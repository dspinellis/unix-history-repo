#	@(#)Makefile	5.2 (Berkeley) %G%

MAN1=	lorder.0

all lorder: ${MAN1}

clean depend lint tags:

cleandir:
	rm -f ${MAN1}

install: maninstall
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/lorder.sh ${DESTDIR}/${BINDIR}/lorder

.include <bsd.own.mk>
.include <bsd.man.mk>
