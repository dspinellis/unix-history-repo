#	@(#)Makefile	8.3 (Berkeley) %G%

DIR=	usd/29.diction
SRCS=	rm0 rm1 rm2 ap1 ap2
MACROS=	-ms

paper.ps: ${SRCS}
	${TBL} ${SRCS} | ${EQN} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
