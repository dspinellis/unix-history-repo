#	@(#)Makefile	6.3 (Berkeley) %G%

DIR=	ps2/01.cacm
SRCS=	p.mac p1 p2 p3 p4 p5 p6
MACROS=	-ms
REFER=	refer -p /usr/old/dict/papers/Ind -e

paper.${PRINTER}: ${SRCS}
	${REFER} ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
