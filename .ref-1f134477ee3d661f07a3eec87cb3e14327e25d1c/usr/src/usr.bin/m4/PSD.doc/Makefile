#	@(#)Makefile	8.1 (Berkeley) %G%

DIR=	psd/17.m4
SRCS=	m4.ms
MACROS=	-msU

paper.ps: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
