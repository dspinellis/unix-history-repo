#	@(#)Makefile	5.4 (Berkeley) %G%

DIR=	smm/14.fastfs
SRCS=	0.t 1.t 2.t 3.t 4.t 5.t 6.t
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${EQN} | ${TROFF} > ${.TARGET}

.include <bsd.doc.mk>
