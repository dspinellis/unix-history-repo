#	@(#)Makefile	8.1 (Berkeley) %G%

DIR=	psd/11.adb
SRCS=	adb.ms
MACROS=	-ms

paper.ps: ${SRCS}
	${EQN} ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
