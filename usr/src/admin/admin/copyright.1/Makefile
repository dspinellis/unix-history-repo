#	@(#)Makefile	5.3 (Berkeley) %G%

BUILD=	redist.c redist.sh redist.man redist.gas
STD=	386.c contrib hdrs kern libc libs libs.cci main make man sh src

all: ${BUILD}

clean cleandir:
	rm -f redist.c redist.sh redist.man redist.gas

install:
	cd ${.CURDIR}; install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${STD} \
	    ${DESTDIR}${BINDIR}/copyright
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${BUILD} \
	    ${DESTDIR}${BINDIR}/copyright

depend lint tags:

redist.c: redist
	sed 's/^/ */' < ${.CURDIR}/redist > redist.c

redist.sh: redist
	sed 's/^/#/' < ${.CURDIR}/redist > redist.sh

redist.man: redist
	sed 's/^/.\\"/' < ${.CURDIR}/redist > redist.man

redist.gas: redist
	sed 's/^/|/' < ${.CURDIR}/redist > redist.gas

.include <bsd.prog.mk>
