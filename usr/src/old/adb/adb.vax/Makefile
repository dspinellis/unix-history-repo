#	@(#)Makefile	5.8 (Berkeley) %G%

PROG=	adb
SRCS=	access.c command.c expr.c format.c kstack.c machdep.c main.c \
	opset.c optab.c output.c pcs.c print.c rodata.c setup.c strings.c \
	sym.c
ASPATH=	../../../pgrm/as.vax
.PATH:	${.CURDIR}/../common_source
CLEANFILES=instrs.adb

CFLAGS+= -I. -I${.CURDIR} -I${.CURDIR}/../common_source -I${.CURDIR}/${ASPATH}

optab.o: instrs.adb

optab.o rodata.o strings.o:
	${CC} ${CFLAGS} -R -c ${.IMPSRC}

INSTR=${.CURDIR}/${ASPATH}/instrs
instrs.adb: ${INSTR}
	(echo FLAVOR ADB; cat ${INSTR}) | awk -f ${INSTR} > ${.TARGET}

.include "../../Makefile.inc"
.include <bsd.prog.mk>

.depend: instrs.adb
