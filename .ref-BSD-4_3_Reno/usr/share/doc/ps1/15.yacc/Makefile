#	@(#)Makefile	6.2 (Berkeley) 6/30/90

DIR=	ps1/15.yacc
SRCS=	ss.. ss0 ss1 ss2 ss3 ss4 ss5 ss6 ss7 ss8 ss9 ssA ssB ssa ssb ssc ssd
MACROS=	-msU
REFER=	refer -e -p /usr/doc/run/Ind

paper.${PRINTER}: ${SRCS}
	${REFER} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
