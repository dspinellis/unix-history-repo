#       @(#)Makefile	5.1 (Berkeley) %G%

DIR=	usd/01.begin
SRCS=	u.mac u0 u1 u2 u3 u4 u5
MACROS= -msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
