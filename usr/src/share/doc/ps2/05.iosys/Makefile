#	@(#)Makefile	5.2 (Berkeley) 6/30/90

DIR=	ps2/05.iosys
SRCS=	iosys
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
