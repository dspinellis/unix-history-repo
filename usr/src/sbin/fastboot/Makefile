#	@(#)Makefile	5.4 (Berkeley) %G%

MAN8=	fastboot.0

all fastboot: ${MAN8}

clean cleandir depend lint tags:

install: maninstall
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/fastboot.sh ${DESTDIR}${BINDIR}/fastboot
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/fasthalt.sh ${DESTDIR}${BINDIR}/fasthalt

.include <bsd.prog.mk>
