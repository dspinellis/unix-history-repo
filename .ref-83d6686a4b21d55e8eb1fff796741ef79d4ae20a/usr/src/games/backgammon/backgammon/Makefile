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
#	@(#)Makefile	5.14 (Berkeley) %G%
#
LIBC=	/lib/libc.a
LIBS=	-ltermlib
CFLAGS=	-O -DV7
SRCS=	allow.c board.c check.c data.c extra.c fancy.c init.c main.c move.c \
	odds.c one.c save.c subs.c table.c teach.c text.c ttext1.c ttext2.c \
	tutor.c version.c
BSRCS=	allow.c board.c check.c extra.c fancy.c init.c main.c move.c \
	odds.c one.c save.c subs.c table.c text.c version.c
BOBJS=	allow.o board.o check.o extra.o fancy.o init.o main.o move.o \
	odds.o one.o save.o subs.o table.o text.o version.o
TSRCS=	allow.c board.c check.c data.c fancy.c init.c odds.c one.c save.c \
	subs.c table.c teach.c ttext1.c ttext2.c tutor.c
TOBJS=	allow.o board.o check.o data.o fancy.o init.o odds.o one.o save.o \
	subs.o table.o teach.o ttext1.o ttext2.o tutor.o
MAN=	backgammon.0

all: backgammon teachgammon

backgammon: ${BOBJS} ${LIBC}
	${CC} -o $@ ${BOBJS} ${LIBS}

teachgammon: ${TOBJS} ${LIBC}
	${CC} -o $@ ${TOBJS} ${LIBS}

clean:
	rm -f ${TOBJS} ${BOBJS} core teachgammon backgammon

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o games -g bin -m 700 backgammon ${DESTDIR}/usr/games/hide
	install -s -o games -g bin -m 700 teachgammon ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f backgammon; ln -s dm backgammon; chown games.bin backgammon)
	(cd ${DESTDIR}/usr/games; rm -f teachgammon; ln -s dm teachgammon; chown games.bin teachgammon)
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6

lint: ${SRCS}
	lint ${CFLAGS} ${BRCS}
	lint ${CFLAGS} ${TSRCS}

tags: ${SRCS}
	ctags ${SRCS}
