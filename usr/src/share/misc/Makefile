#	@(#)Makefile	5.7 (Berkeley) %G%

FILES=	airport ascii birthday eqnchar greek inter.phone man.template \
	na.phone operator

all: ${FILES}

clean cleandir:
	rm -f redist.c redist.sh redist.man redist.gas

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

redist.gas: redist
	rm -f redist.gas
	sed 's/^/|/' < redist > redist.gas
	chmod 444 redist.gas

.include <bsd.own.mk>
