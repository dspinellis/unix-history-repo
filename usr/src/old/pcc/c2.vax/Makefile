#	@(#)Makefile	4.10 (Berkeley) %G%

PROG=	c2
SRCS=	c20.c c21.c c22.c
CFLAGS+=-DCOPYCODE -DC2
ASDIR=	${.CURDIR}/../../pgrm/as.${MACHINE}
NOMAN=	noman

c22.o: instrs.c2
	${CC} ${CFLAGS} -R -c ${.IMPSRC}

instrs.c2: ${ASDIR}/instrs
	(echo FLAVOR C2 ; cat ${ASDIR}/instrs) | \
	    awk -f ${ASDIR}/instrs > instrs.c2

.include <bsd.prog.mk>
