#	@(#)Makefile	6.2 (Berkeley) %G%

DIR=	smm/09.uucpimpl
SRCS=	implement.ms
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
