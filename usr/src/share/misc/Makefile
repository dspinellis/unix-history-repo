#	@(#)Makefile	5.4 (Berkeley) %G%

FILES=	airport ascii birthday eqnchar greek inter.phone man.template \
	na.phone noredist.c operator redist.c redist.man redist.sh

all: ${FILES}
.include <bsd.own.mk>

clean cleandir:
	rm -f redist.c redist.sh redist.man

install:
	cd ${.CURDIR}; install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}/misc

depend lint tags:

redist.c: redist
	rm -f redist.c
	sed 's/^/ */' < redist > redist.c
	chmod 444 redist.c

redist.sh: redist
	rm -f redist.sh
	sed 's/^/#/' < redist > redist.sh
	chmod 444 redist.sh

redist.man: redist
	rm -f redist.man
	sed 's/^/.\\"/' < redist > redist.man
	chmod 444 redist.man
