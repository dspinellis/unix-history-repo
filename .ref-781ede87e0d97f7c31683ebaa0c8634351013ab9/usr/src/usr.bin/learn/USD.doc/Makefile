#	@(#)Makefile	6.2 (Berkeley) %G%

DIR=	usd/02.learn
SRCS=	p0 p1 p2 p3 p4 p5 p6 p7
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${REFER} -p Refs -e ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
