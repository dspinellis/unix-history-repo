#	@(#)Makefile	6.2 (Berkeley) %G%

DIR=	smm/20.termdesc
SRCS=	termdesc.me
MACROS=	-me

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
