#	@(#)Makefile	8.1 (Berkeley) 8/14/93

DIR=	usd/03.shell
SRCS=	t.mac t1 t2 t3 t4
MACROS= -ms
REFER=  refer -e -p /usr/old/dict/papers/Ind

paper.ps: ${SRCS}
	${REFER} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
