#	@(#)Makefile	6.3 (Berkeley) %G%

DIR=	usd/02.learn
SRCS=	p0 p1 p2 p3 p4 p5 p6 p7
MACROS=	-msU

paper.ps: ${SRCS}
	${REFER} -p Refs -e ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
