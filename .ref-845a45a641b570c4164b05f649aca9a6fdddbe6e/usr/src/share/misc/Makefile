#	@(#)Makefile	5.14 (Berkeley) %G%

FILES=	airport ascii birthtoken eqnchar flowers inter.phone man.template \
	mdoc.template na.phone operator zipcodes
NOOBJ=	noobj

all clean cleandir depend lint tags:

install:
	cd ${.CURDIR}; install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}/misc

.include <bsd.prog.mk>
