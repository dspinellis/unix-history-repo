#	@(#)Makefile	5.4 (Berkeley) %G%

SRCS=	usenix.me
MACROS=	-me

all: usenix.${PRINTER}

usenix.${PRINTER}: ${SRCS}
	${PIC} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
