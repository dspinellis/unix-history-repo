#	@(#)Makefile	8.1 (Berkeley) %G%

NOMAN=noman
NOOBJ=noobj

all nologin clean cleandir depend lint tags:

beforeinstall:
.if ${MACHINE} == "sparc"
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/cpp.notraditional.sh ${DESTDIR}/usr/bin/cpp
.else
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/cpp.sh ${DESTDIR}/usr/bin/cpp
.endif

.include <bsd.prog.mk>
