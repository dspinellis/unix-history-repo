#	@(#)Makefile	5.5 (Berkeley) %G%

DIR=	smm/16.sendmail
SRCS=	intro.me
MACROS=	-me

all: intro.${PRINTER}

intro.ps: ${SRCS}
	${PIC} ${SRCS} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
