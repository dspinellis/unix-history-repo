#	@(#)Makefile	8.1 (Berkeley) %G%

DIR=	papers/nqnfs
SRCS=	nqnfs.me
MACROS=	-me

paper.ps: ${SRCS}
	${PIC} ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
