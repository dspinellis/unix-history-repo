#
#	Makefile	4.4	86/06/07
#
PROG=	adb
SRCS=	access.c command.c expr.c format.c input.c main.c message.c \
	opset.hp300.c optab.hp300.c output.c pcs.c print.c \
	runpcs.c setup.c sym.c
ASPATH=	../../../usr.bin/gas
SYSPATH=../../../sys/hp300

CC=	cc -traditional
INCS= -I. -I${.CURDIR} -I${.CURDIR}/${SYSPATH}
DFLAGS = -DADB -DNEWVM
CFLAGS+= -w -g ${DFLAGS} ${INCS}

optab.hp300.o message.o:
	${CC} ${CFLAGS} -R -c ${.IMPSRC}

.include "../../Makefile.inc"
.include <bsd.prog.mk>
