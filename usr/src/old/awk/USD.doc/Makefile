#	@(#)Makefile	5.3 (Berkeley) %G%

DIR=	usd/16.awk
SRCS=	awk
MACROS=	-msU

paper.ps: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
