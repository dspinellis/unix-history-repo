#	@(#)Makefile	8.1 (Berkeley) 6/8/93

DIR=	psd/09.f77io
SRCS=	f77IO.ms
MACROS=	-msU

paper.ps: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
