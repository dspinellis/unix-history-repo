#	@(#)Makefile	5.1 (Berkeley) %G%

DIR=	usd/27.eqnguide
SRCS=	g.mac g0 g1 g2 g3 g4 g5
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${EQN} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
