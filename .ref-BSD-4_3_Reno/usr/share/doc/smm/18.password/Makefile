#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	smm/18.password
SRCS=	password.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${EQN} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
