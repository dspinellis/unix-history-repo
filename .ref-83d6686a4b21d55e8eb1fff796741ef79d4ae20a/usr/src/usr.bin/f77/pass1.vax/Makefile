#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5	(Berkeley)	%G%
#
CFLAGS=	-O -DHERE=VAX -DTARGET=VAX -DFAMILY=PCC -DUCBVAXASM \
	-DUCBPASS2 -DNAMELIST -I../include
LIBC=	/lib/libc.a
SRCS=	bb.c conv.c data.c equiv.c error.c exec.c expr.c fmt.c gram.c init.c \
	intr.c io.c lex.c main.c misc.c optcse.c optim.c optloop.c paramset.c \
	proc.c put.c putpcc.c regalloc.c setfmt.c stab.c vax.c vaxx.c
OBJS=	bb.o conv.o data.o equiv.o error.o exec.o expr.o fmt.o gram.o init.o \
	intr.o io.o lex.o main.o misc.o optcse.o optim.o optloop.o paramset.o \
	proc.o put.o putpcc.o regalloc.o setfmt.o stab.o vax.o vaxx.o

all: f77pass1

f77pass1: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

gram.c:	gram.head gram.dcl gram.expr gram.exec gram.io tokdefs.h
	( sed <tokdefs.h "s/#define/%token/"; \
		cat gram.head gram.dcl gram.expr gram.exec gram.io ) >gram.in
	${YACC} gram.in
	mv y.tab.c gram.c

gram.head: defs.h data.h

tokdefs.h: tokens
	grep -n . <tokens | sed "s/\([^:]*\):\(.*\)/#define \2 \1/" >tokdefs.h

clean:
	rm -f ${OBJS} core f77pass1 gram.c gram.in

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 f77pass1 ${DESTDIR}/usr/libexec

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
