#	@(#)Makefile	1.3 (Berkeley) %G%

DIR=	papers/future
SRCS=	0.t 1.t 2.t r.t
MACROS=	-ms

paper.ps: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
