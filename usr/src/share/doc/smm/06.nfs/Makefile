#	@(#)Makefile	5.1 (Berkeley) %G%

DIR=	papers/nfs
SRCS=	nfs-notes.me
MACROS=	-me

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}
.include <bsd.doc.mk>
