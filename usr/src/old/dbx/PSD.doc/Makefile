#	@(#)Makefile	6.2 (Berkeley) %G%

DIR=	ps1/11.dbx
SRCS=	dbx.ms
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
