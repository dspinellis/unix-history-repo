#
# Copyright (c) 1987 The Regents of the University of California.
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
#	@(#)Makefile	4.10 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
YFLAGS=	-d
OBJS=	awk.lx.o b.o main.o token.o tran.o lib.o run.o parse.o proctab.o \
	freeze.o
SRCS=	awk.lx.c b.c lib.c main.c parse.c proctab.c freeze.c run.c \
	token.c tran.c
MAN=	awk.0

all: awk

awk: ${OBJS} awk.g.o ${LIBC}
	${CC} ${CFLAGS} awk.g.o  ${OBJS} -lm -o $@

y.tab.h: awk.g.o

awk.h: y.tab.h
	-cmp -s y.tab.h awk.h || cp y.tab.h awk.h

token.c: awk.h
	ex - < tokenscript

profile: awk.g.o ${OBJS} mon.o
	${CC} -p -i awk.g.o ${OBJS} mon.o -lm

proctab.c: proc
	./proc > proctab.c

proc: awk.h proc.o token.o
	${CC} -o $@ proc.o token.o

clean:
	rm -f *.o core awk awk.h awk.lx.c y.tab.h proc proctab.c yacc* temp

cleandir: clean
	rm -f ${MAN} tags .depend

depend: y.tab.h ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 awk ${DESTDIR}/usr/bin
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
