#	@(#)Makefile	5.3 (Berkeley) 5/18/90

MAN1=	lorder.0

all lorder: ${MAN1}

clean depend lint tags:

cleandir:
	rm -f ${MAN1}

install: maninstall
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/lorder.sh ${DESTDIR}/${BINDIR}/lorder

.include <bsd.prog.mk>
.include <bsd.man.mk>
