#	@(#)Makefile	6.2 (Berkeley) %G%

DIR=	ps1/09.lint
SRCS=	lint.ms
MACROS=	-msU
REFER=	refer -e -p /usr/doc/run/Ind

paper.${PRINTER}: ${SRCS}
	${REFER} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
