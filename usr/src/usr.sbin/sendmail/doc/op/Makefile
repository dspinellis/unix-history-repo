#	@(#)Makefile	8.1 (Berkeley) %G%

DIR=	smm/07.sendmailop
SRCS=	op.me
MACROS=	-me

all: op.ps

op.ps: ${SRCS}
	${PIC} ${SRCS} | ${EQN} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
