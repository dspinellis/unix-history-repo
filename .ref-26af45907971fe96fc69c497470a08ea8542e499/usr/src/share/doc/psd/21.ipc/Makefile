#	@(#)Makefile	1.5 (Berkeley) %G%

DIR=	psd/20.ipc
SRCS=	0.t 1.t 2.t 3.t 4.t 5.t
MACROS=	-ms

paper.ps: ${SRCS}
	${TBL} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
