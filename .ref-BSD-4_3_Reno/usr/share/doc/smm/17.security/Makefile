#	@(#)Makefile	1.2 (Berkeley) 6/30/90

DIR=	smm/17.security
SRCS=	security.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	 ${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
