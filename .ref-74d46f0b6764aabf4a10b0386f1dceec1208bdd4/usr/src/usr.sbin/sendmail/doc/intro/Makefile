#	@(#)Makefile	5.3 (Berkeley) %G%

DIR=	smm/16.sendmail
SRCS=	intro.me
MACROS=	-me

paper.${PRINTER}: ${SRCS}
	${PIC} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
