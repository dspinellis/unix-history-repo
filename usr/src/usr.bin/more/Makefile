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
# @(#)Makefile	5.2 (Berkeley) %G%
#

DEFS=	-DHELPFILE=\"/usr/lib/less.help\" -DEDIT_PGM=\"/usr/ucb/vi\" -DRECOMP
CFLAGS=	-O ${DEFS}
LIBC=	/lib/libc.a
SRCS=	ch.c command.c decode.c help.c input.c line.c linenum.c main.c \
	option.c os.c output.c position.c prim.c prompt.c screen.c \
	signal.c tags.c ttyin.c
OBJS=	ch.o command.o decode.o help.o input.o line.o linenum.o main.o \
	option.o os.o output.o position.o prim.o prompt.o screen.o \
	signal.o tags.o ttyin.o
MAN=	less.0

all: less

less: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} -ltermcap

#funcs.h: ${SRCS}
funcs.h:
	awk -f mkfuncs.awk ${SRCS} > funcs.h

clean:
	rm -f ${OBJS} core less lesskey

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS} funcs.h
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 less ${DESTDIR}/usr/ucb/less
	install -o bin -g bin -m 444 less.0 ${DESTDIR}/usr/man/cat1/less.0
	install -c -o bin -g bin -m 444 less.help ${DESTDIR}/usr/lib/less.help

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
