#	@(#)Makefile	5.2 (Berkeley) %G%

PROG=	adb
SRCS=	access.c command.c expr.c format.c input.c main.c message.c \
	opset.hp300.c optab.hp300.c output.c pcs.c print.c runpcs.c \
	setup.c sym.c
.PATH:	${.CURDIR}/../common_source
DFLAGS=	-DADB -DNEWVM
CC+=	-traditional
CFLAGS+=-w -g ${DFLAGS} -I. -I${.CURDIR} -I${.CURDIR}/../../../sys/hp300

optab.hp300.o message.o:
	${CC} ${CFLAGS} -R -c ${.IMPSRC}

.include "../../Makefile.inc"
.include <bsd.prog.mk>
