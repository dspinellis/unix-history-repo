#	@(#)Makefile	6.3 (Berkeley) 6/30/90

DIR=	smm/12.uchanges
SRCS=	0.t 1.t 2.t 3.t 4.t 5.t 6.t 7.t 8.t a.t
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
