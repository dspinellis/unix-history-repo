#	@(#)Makefile	5.9 (Berkeley) %G%

FILES=	airport ascii birthday eqnchar greek inter.phone man.template \
	na.phone operator
NOOBJ=	noobj

all clean cleandir depend lint tags:

install:
	cd ${.CURDIR}; install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}/misc

.include <bsd.prog.mk>
