#	@(#)Makefile	5.2 (Berkeley) %G%

ALL=	op.ps intro.ps usenix.ps

all: ${ALL}

op.ps: op.me
	${PIC} op.me | ${EQN} | ${ROFF} -me > ${.TARGET}

intro.ps: intro.me
	${PIC} intro.me | ${ROFF} -me > ${.TARGET}

usenix.ps: usenix.me
	${ROFF} -me usenix.me > ${.TARGET}

.include <bsd.doc.mk>
