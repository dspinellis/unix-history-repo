#	@(#)Makefile	8.1 (Berkeley) 6/8/93

DIR=	usd/27.invert
SRCS=	refer
MACROS=	-msU

paper.ps: ${SRCS}
	${REFER} ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
