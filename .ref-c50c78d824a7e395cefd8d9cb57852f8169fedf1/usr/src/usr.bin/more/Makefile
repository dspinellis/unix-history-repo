#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation, advertising
# materials, and other materials related to such redistribution and
# use acknowledge that the software was developed by the University
# of California, Berkeley.  The name of the University may not be
# used to endorse or promote products derived from this software
# without specific prior written permission.  THIS SOFTWARE IS PROVIDED
# ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
# WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND
# FITNESS FOR A PARTICULAR PURPOSE.
#
# @(#)Makefile	5.3 (Berkeley) %G%
#

CFLAGS=	-O -I/nbsd/usr/include -I.
LIBC=	/lib/libc.a
SRCS=	ch.c command.c decode.c help.c input.c line.c linenum.c main.c \
	option.c os.c output.c position.c prim.c screen.c \
	signal.c tags.c ttyin.c
OBJS=	ch.o command.o decode.o help.o input.o line.o linenum.o main.o \
	option.o os.o output.o position.o prim.o screen.o \
	signal.o tags.o ttyin.o
MAN=	more.0

all: more

more: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} -ltermcap /nbsd/lib/libc.a

clean:
	rm -f ${OBJS} core more

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 more ${DESTDIR}/usr/ucb/more
	install -o bin -g bin -m 444 more.0 ${DESTDIR}/usr/man/cat1
	install -c -o bin -g bin -m 444 more.help ${DESTDIR}/usr/lib

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
