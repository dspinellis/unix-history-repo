#	@(#)Makefile	5.1 (Berkeley) %G%

FILES=	3101 aa aed512 beehive diablo dtc382 hp700-wy ibm3101 std stdcrt \
	tandem653 teleray vt100 wyse-adds xerox1720 xerox1730 xerox1730-lm \
	zenith29

all clean cleandir depend lint tags:

install:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}/tabset

.include <bsd.own.mk>
