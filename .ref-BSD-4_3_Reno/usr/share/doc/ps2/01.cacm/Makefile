#	@(#)Makefile	6.3 (Berkeley) 6/30/90

DIR=	ps2/01.cacm
SRCS=	p.mac p1 p2 p3 p4 p5 p6
MACROS=	-msU
REFER=	refer -e

paper.${PRINTER}: ${SRCS}
	${REFER} ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
