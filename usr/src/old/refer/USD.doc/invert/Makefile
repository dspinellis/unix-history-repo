#	@(#)Makefile	5.1 (Berkeley) %G%

DIR=	usd/30.invert
SRCS=	refer
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${REFER} ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
