#	@(#)Makefile	4.8 (Berkeley) %G%

PROG=	quiz
MAN6=	quiz.0
CATS=	africa america areas arith asia babies bard chinese collectives \
	ed elements europe greek inca index latin locomotive midearth morse \
	murders poetry posneg pres province seq-easy seq-hard sexes sov \
	spell state trek ucc
HIDEGAME=hidegame

beforeinstall:
	(cd ${.CURDIR}/datfiles; install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
	    ${CATS} ${DESTDIR}/usr/share/games/quiz.db)

.include <bsd.prog.mk>
