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
#	@(#)Makefile	5.3 (Berkeley) %G%
#
COMMON=	../common_source
CFLAGS=	-O -DNEQN -I. -I${COMMON}
YFLAGS=	-d
VPATH=	${COMMON}
LIBC=	/lib/libc.a
SRCS=	e.c diacrit.c eqnbox.c font.c fromto.c funny.c glob.c integral.c \
	io.c lex.c lookup.c mark.c matrix.c move.c over.c paren.c pile.c \
	shift.c size.c sqrt.c text.c
OBJS=	e.o diacrit.o eqnbox.o font.o fromto.o funny.o glob.o integral.o \
	io.o lex.o lookup.o mark.o matrix.o move.o over.o paren.o pile.o \
	shift.o size.o sqrt.o text.o

all: neqn

neqn: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

e.c: ${COMMON}/e.y
	${YACC} ${YFLAGS} ${COMMON}/e.y
	mv y.tab.h e.def
	mv y.tab.c e.c

clean:
	rm -f ${OBJS} core neqn e.c e.def

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 neqn ${DESTDIR}/usr/bin

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
