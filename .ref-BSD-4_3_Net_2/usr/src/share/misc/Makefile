#	@(#)Makefile	5.13 (Berkeley) 5/7/91

FILES=	airport ascii birthtoken eqnchar inter.phone man.template \
	mdoc.template na.phone operator zipcodes
NOOBJ=	noobj

all clean cleandir depend lint tags:

install:
	cd ${.CURDIR}; install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}/misc

.include <bsd.prog.mk>
