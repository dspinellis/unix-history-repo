#
# Copyright (c) 1989 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# Edward Estes.
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
#	@(#)Makefile	5.6 (Berkeley) %G%
#
# PLOTDEVICE is used for plotting the map.
# Change as appropriate.
PLOTDEVICE =	> /dev/tty

# The following are program constants which are implementation dependent.
#
# DEST is passed on.  Note that there is no closing quote here.
# WIZARD is the login of the one who will clean up things.
# UID is the uid of game wizard.
# define OK_TO_PLAY to restrict playing access at certain times.
#	Also see function ok_to_play() in main.c, and tailor to your own needs.
# define ENEMY to include code for checking of a 'hit list' of resricted 
#	accounts.  The list of logins goes in the file 'enemy'.
# SHELL is the default shell for shell escapes.  If SHELL is not defined,
#	shell escapes are not enabled.

DEST=	/usr/games/lib/phantasia
DESTR=	/usr/games
CFLAGS=	-O -DDEST=\"${DEST} -DDESTR=\"${DESTR} -DWIZARD=\"root\" -DUID=0 \
	-DENEMY -DBSD42
LIBC=	/lib/libc.a
SRCS=	main.c fight.c io.c interplayer.c gamesupport.c misc.c phantglobs.c
OBJS=	main.o fight.o io.o interplayer.o gamesupport.o misc.o phantglobs.o
MAN=	phantasia.0

# Make documentation.
# The flags below on tbl and nroff are to make a line printable version.
.6.0:
	tbl -TX phantasia.6 | /usr/man/manroff > phantasia.0

all: phantasia setup

phantasia: ${OBJS} ${LIBC}
	${CC} ${OBJS} -o $@ -lm -lcurses -ltermlib

# Make the initialization program.
setup: phantglobs.o setup.o monsters.asc ${LIBC}
	${CC} phantglobs.o setup.o -o $@ -lm -lcurses -ltermlib

install: ${MAN}
	-[ -d ${DESTDIR}/usr/games/lib/phantasia ] || mkdir ${DESTDIR}/usr/games/lib/phantasia
	chmod 700 /usr/games/lib/phantasia
	chown games.bin /usr/games/lib/phantasia
	install -s -o games -g bin -m 4700 phantasia ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f phantasia; ln -s dm phantasia; chown games.bin phantasia)
	install -c -o games -g bin -m 400 phantasia.0 ${DESTDIR}/usr/games/lib/phantasia/phant.help
	install -c -o bin -g bin -m 444 phantasia.0 ${DESTDIR}/usr/man/cat6
	./setup
	chown games.bin /usr/games/lib/phantasia/*

# Make Phantasia map.
# Change the map commands reflect your installation.
# Pre-System III installations may use 'plot' instead of 'tplot'.
map: map.c
	${CC} -O map.c -lplot -o $@
	map | tplot ${PLOTDEVICE}

# Convert old character file to new format.
# 'convert' is meant to be run only once.
# PLUS = PHANTPLUS for conversion from 3.3.1+.
convert: convert.c oldplayer.h phantstruct.h
	${CC} ${CFLAGS} ${PLUS} convert.c ${LDFLAGS} -o convert
	convert
	@echo "New data is in newcharacs."

clean:
	rm -f *.o core phantasia phant.help phantasia.0 setup

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
