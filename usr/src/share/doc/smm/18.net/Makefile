#	@(#)Makefile	6.3 (Berkeley) %G%

DIR=	smm/15.net
SRCS=	0.t 1.t 2.t 3.t 4.t 5.t 6.t 7.t 8.t 9.t a.t b.t c.t d.t e.t f.t
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
