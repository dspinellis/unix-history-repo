#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	ps1/16.lex
SRCS=	lex.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
