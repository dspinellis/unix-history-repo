#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	smm/09.uucpimpl
SRCS=	implement.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
