#	@(#)Makefile	5.2 (Berkeley) 6/30/90

DIR=	ps1/02.f77
SRCS=	f77.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${EQN} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
