#
# Copyright (c) 1989 The Regents of the University of California.
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
#	@(#)Makefile	5.6 (Berkeley) %G%
#
# Configuration options
#  
#  LARNHOME is the directory where the larn data files will be installed.
#  BINDIR is the directory where the larn binary will be installed.

LARNHOME=	/usr/games/lib/larnfiles
BINDIR=		/usr/games

# Available compile time options:
# WIZID=xxx
#	This is the userid (or playerid) of the wizard.  Default is zero
#	(superuser), which disables all wizard functions.  Players must
#	have this userid (or playerid) in order to become the non-scoring
#	wizard player.  Definition of WIZID to non-zero will enable the
#	special wizard debugging commands.  For root to become wizard, use
#	WIZID= -1.
# EXTRA
#	Incorporates code to gather additional performance statistics
# TIMECHECK
#	Incorporates code to disable play during working hours (8-5)
# SYSV
#	Use system III/V (instead of V7) type ioctl calls
# BSD
#	Use BSD specific features (mostly timer and signal stuff)
# BSD4.1
#	Use BSD4.1 to avoid some 4.2 dependencies (must be used with
#	BSD above; do not mix with SYSV)
# HIDEBYLINK
#	If defined, the program attempts to hide from ps
# DOCHECKPOINTS
#	If not defined, checkpoint files are periodically written by the
#	larn process (no forking) if enabled in the .larnopts description
#	file.  Checkpointing is handy on an unreliable system, but takes
#	CPU. Inclusion of DOCHECKPOINTS will cause fork()ing to perform the
#	checkpoints (again if enabled in the .larnopts file).  This usually
#	avoids pauses in larn while the checkpointing is being done (on
#	large machines).
# SAVEINHOME
#	Put save files in users HOME instead of LARNHOME the as default
# VER
#	This is the version of the software, example:  12
# SUBVER
#	This is the revision of the software, example:  1
# FLUSHNO=#
#	Set the input queue excess flushing threshold (default 5)
# NOVARARGS
#	Define for systems that don't have varargs (a default varargs will
#	be used).
# MACRORND
#	Define to use macro version of rnd() and rund() (fast and big)
# UIDSCORE
#	Define to use user id's to manage scoreboard.  Leaving this out will
#	cause player id's from the file ".playerids" to be used instead.
#	(.playerids is created upon demand).  Only one entry per id # is
#	allowed in each scoreboard (winning & non-winning).
#  VT100
#	Compile for using vt100 family of terminals.  Omission of this
#	define will cause larn to use termcap, but it will be MUCH slower
#	due to an extra layer of output interpretation.  Also, only VT100
#	mode allows 2 different standout modes, inverse video, and bold video.
#	And only in VT100 mode is the scrolling region of the terminal used
#	(much nicer than insert/delete line sequences to simulate it, if
#	VT100 is omitted).
# NONAP
#	This causes napms() to return immediately instead of delaying n
#	milliseconds.  This define may be needed on some systems if the nap
#	stuff does not work correctly (possible hang).  nap() is primarilly
#	used to delay for effect when casting missile type spells.
# NOLOG
#	Turn off logging.
#

OPTIONS = -DBSD -DVER=12 -DSUBVER=0 -DWIZID=-7 -DSAVEINHOME -DNONAP -DNOLOG

SRCS=	main.c object.c create.c tok.c display.c global.c data.c io.c \
	monster.c store.c diag.c help.c config.c nap.c bill.c scores.c \
	signal.c moreobj.c movem.c regen.c fortune.c savelev.c
OBJS=	main.o object.o create.o tok.o display.o global.o data.o io.o \
	monster.o store.o diag.o help.o config.o nap.o bill.o scores.o \
	signal.o moreobj.o movem.o regen.o fortune.o savelev.o

CFLAGS= -O ${OPTIONS} -DLARNHOME=\"${LARNHOME}/\"

all: larn

larn: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} ${OBJS} -o $@ -ltermcap

install: ${MAN}
	-[ -d ${DESTDIR}/usr/games/lib/larnfiles ] || mkdir ${DESTDIR}/usr/games/lib/larnfiles
	chmod 700 ${DESTDIR}/usr/games/lib/larnfiles
	chown games.src ${DESTDIR}/usr/games/lib/larnfiles
	install -s -o games -g bin -m 4700 larn ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f larn; ln -s dm larn; chown games.bin larn)
	install -c -o games -g bin -m 444 larnmaze larnopts lfortune \
		larn.help ${DESTDIR}/usr/games/lib/larnfiles

clean:
	rm -f ${OBJS} core larn

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
