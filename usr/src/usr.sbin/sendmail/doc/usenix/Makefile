#	@(#)Makefile	8.1 (Berkeley) %G%

SRCS=	usenix.me
MACROS=	-me

all: usenix.ps

usenix.ps: ${SRCS}
	${PIC} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
