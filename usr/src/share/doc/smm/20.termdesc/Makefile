#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	smm/20.termdesc
SRCS=	termdesc.me
MACROS=	-me

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
