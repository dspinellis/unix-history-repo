#	@(#)Makefile	6.3 (Berkeley) %G%

DIR=	smm/06.lpd
SRCS=	0.t 1.t 2.t 3.t 4.t 5.t 6.t 7.t
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
