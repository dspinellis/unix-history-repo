#
# Copyright (c) 1987 Regents of the University of California.
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
#	@(#)Makefile	5.8 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
LIBS=	-lcurses -ltermlib
SRCS=	comp.c end.c extern.c init.c mille.c misc.c move.c print.c \
	roll.c save.c table.c types.c varpush.c
MSRCS=	comp.c end.c extern.c init.c mille.c misc.c move.c print.c \
	roll.c save.c types.c varpush.c
MOBJS=	comp.o end.o extern.o init.o mille.o misc.o move.o print.o \
	roll.o save.o types.o varpush.o
TSRCS=	table.c extern.c
TOBJS=	table.o extern.o
MAN=	mille.0

all: mille

mille: ${MOBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${MOBJS} ${LIBS}

table: ${TOBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${TOBJS}

clean:
	rm -f ${OBJS} ${TOBJS} ${MOBJS} core mille table

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o games -g bin -m 700 mille ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f mille; ln -s dm mille; chown games.bin mille)
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6

lint: ${SRCS}
	lint ${CFLAGS} ${MSRCS}
	lint ${CFLAGS} ${TSRCS}

tags: ${SRCS}
	ctags ${MSRCS}
	ctags -a ${TSRCS}
	sort -o tags tags
