#	@(#)Makefile	6.2 (Berkeley) %G%

DIR=	ps1/10.adb
SRCS=	adb.ms
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	${EQN} ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
