#	@(#)Makefile	6.2 (Berkeley) %G%

DIR=	ps1/01.Clang
SRCS=	Clang.ms
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
