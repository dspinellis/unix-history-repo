#	@(#)Makefile	5.3 (Berkeley) %G%

DIR=	psd/08.f77
SRCS=	f77.ms
MACROS=	-msU

paper.ps: ${SRCS}
	${TBL} ${SRCS} | ${EQN} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
