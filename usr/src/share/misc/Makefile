#	@(#)Makefile	5.8 (Berkeley) %G%

FILES=	airport ascii birthday eqnchar greek inter.phone man.template \
	na.phone operator

all: ${FILES}

install:
	cd ${.CURDIR}; install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}/misc

clean cleandir depend lint tags:

.include <bsd.own.mk>
