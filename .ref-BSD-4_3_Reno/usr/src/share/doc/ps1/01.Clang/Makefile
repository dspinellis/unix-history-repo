#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	ps1/01.Clang
SRCS=	Clang.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
