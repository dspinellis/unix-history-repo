#	@(#)Makefile	6.2 (Berkeley) %G%

DIR=	ps2/08.ratfor
SRCS=	m.mac m0 m1 m2 m3 m4 m5 m9 m99
MACROS=	-msU

paper.${PRINTER}: ${SRCS}
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
