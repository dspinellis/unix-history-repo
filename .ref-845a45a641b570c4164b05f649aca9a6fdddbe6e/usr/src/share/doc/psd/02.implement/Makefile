#	@(#)Makefile	6.4 (Berkeley) %G%

DIR=	psd/02.implement
SRCS=	implement
MACROS=	-ms
REFER=	refer -p /usr/old/dict/papers/Ind -e

paper.ps: ${SRCS} fig1.pic fig2.pic
	${SOELIM} ${SRCS} | ${REFER} | ${PIC} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
