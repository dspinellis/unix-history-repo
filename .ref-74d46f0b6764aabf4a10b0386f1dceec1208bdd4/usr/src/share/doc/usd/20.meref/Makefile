#	@(#)Makefile	6.4 (Berkeley) %G%

DIR=	usd/23.meref
SRCS=	ref.me
MACROS=	-me

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
