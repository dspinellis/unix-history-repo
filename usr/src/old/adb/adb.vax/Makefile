#
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.4 (Berkeley) %G%
#
LIBC=	/lib/libc.a
AS=	../../as/as.vax
VPATH=	../adb
CFLAGS=	-I. -I../adb -I${AS} -O
SRCS=	access.c command.c expr.c format.c kstack.c machdep.c main.c \
	opset.c optab.c output.c pcs.c print.c rodata.c setup.c strings.c \
	sym.c
OBJS=	access.o command.o expr.o format.o kstack.o machdep.o main.o \
	opset.o optab.o output.o pcs.o print.o rodata.o setup.o strings.o \
	sym.o
MAN=	adb.0

all: adb

adb: ${OBJS} ${LIBC}
	${CC} -o $@ ${OBJS}

optab.o rodata.o strings.o:
	${CC} ${CFLAGS} -c -R $<

instrs.adb: ${AS}/instrs
	(echo FLAVOR ADB; cat ${AS}/instrs) | awk -f ${AS}/instrs > instrs.adb

clean:
	rm -f ${OBJS} core adb

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 adb ${DESTDIR}/bin/adb
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
