#	@(#)Makefile	6.4 (Berkeley) %G%

DIR=	psd/01.cacm
SRCS=	p.mac p1 p2 p3 p4 p5 p6
MACROS=	-ms
REFER=	refer -p /usr/old/dict/papers/Ind -e

paper.ps: ${SRCS}
	${REFER} ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
