#	@(#)Makefile	5.2 (Berkeley) %G%

DIR=	ps2/05.iosys
SRCS=	iosys
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
