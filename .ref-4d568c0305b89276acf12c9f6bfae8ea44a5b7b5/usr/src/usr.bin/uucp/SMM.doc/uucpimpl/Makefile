#	@(#)Makefile	8.1 (Berkeley) %G%

DIR=	smm/14.uucpimpl
SRCS=	implement.ms
MACROS=	-ms

paper.ps: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
