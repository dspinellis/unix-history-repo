#	@(#)Makefile	6.2 (Berkeley) %G%

DIR=	ps1/16.lex
SRCS=	lex.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
