#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	ps1/10.adb
SRCS=	adb.ms
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	${EQN} ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
