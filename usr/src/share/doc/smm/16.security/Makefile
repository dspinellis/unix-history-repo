#	@(#)Makefile	1.2 (Berkeley) %G%

DIR=	smm/17.security
SRCS=	security.ms
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	 ${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
