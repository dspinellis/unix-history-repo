#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.7	(Berkeley)	6/5/87
#
LIBC=	/lib/libc.a
DFLAGS=	-DADB
CFLAGS=	-O -w ${DFLAGS}
#	path to the directory containing as
AS =	../../as/as.vax
SRCS=	access.c command.c expr.c format.c input.c main.c message.c \
	opset.c optab.c output.c pcs.c print.c runpcs.c setup.c sym.c
OBJS=	access.o command.o expr.o format.o input.o main.o message.o \
	opset.o optab.o output.o pcs.o print.o runpcs.o setup.o sym.o

all: adb

adb:	${OBJS} ${LIBC}
	${CC} -o $@ ${OBJS}

message.o:
	${CC} -c -R message.c

opset.o:
	${CC} ${CFLAGS} -c -I. -I${AS} opset.c

optab.o: instrs.adb
	${CC} ${CFLAGS} -c -R -I. -I${AS} optab.c

instrs.adb: ${AS}/instrs
	{echo FLAVOR ADB; cat ${AS}/instrs) | awk -f ${AS}/instrs > instrs.adb

clean: FRC
	rm -f ${OBJS} core adb

depend: FRC
	mkdep ${CFLAGS} -I. -I${AS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 adb ${DESTDIR}/bin/adb

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.
