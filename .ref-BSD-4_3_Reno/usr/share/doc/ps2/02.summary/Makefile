#	@(#)Makefile	6.3 (Berkeley) 6/30/90

DIR=	ps2/02.summary
SRCS=	vhel.mac vhel0 vhel1 vhel2 vhel3 vhel4 vhel5 vhel6
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${EQN} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
