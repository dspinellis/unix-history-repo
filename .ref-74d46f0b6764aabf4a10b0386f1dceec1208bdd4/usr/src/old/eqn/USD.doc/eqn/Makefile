#	@(#)Makefile	5.1 (Berkeley) %G%

DIR=	usd/26.eqn
SRCS=	e.mac e0 e1 e2 e3 e4 e5 e6 e7
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${EQN} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
