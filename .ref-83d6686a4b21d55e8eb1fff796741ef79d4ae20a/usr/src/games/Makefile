#
# Copyright (c) 1980, 1987 Regents of the University of California.
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
#	@(#)Makefile	5.12 (Berkeley) %G%
#
DESTDIR=
CFLAGS=	-O
LIBC=	/lib/libc.a

# Programs that live in subdirectories, and have makefiles of their own.
#
# doctor removed until lisp works
SUBDIR=	adventure atc backgammon battlestar boggle btlgammon canfield \
	cribbage dm fortune hack hangman hunt mille monop \
	phantasia quiz robots rogue sail snake trek \
	games.${MACHINE}

# Shell scripts that need only be installed and are never removed.
#
SCRIPT=	wargames

# C sources that live in the current directory
#
SRCS=	arithmetic.c banner.c bcd.c factor.c fish.c morse.c number.c ppt.c \
	primes.c rain.c worm.c worms.c wump.c

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	arithmetic banner bcd factor fish morse number ppt wump

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	primes rain worm worms

# programs that get hidden
HIDE=	fish wump worm

# programs that don't
NOHIDE=	arithmetic banner bcd factor morse number ppt primes rain worms

# Programs that require a lisp interpreter or compiler
LISP=	doctor

all:	${SUBDIR} ${STD} ${NSTD}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

${STD}: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

install: FRC
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${SCRIPT}; do \
		(install -c -o games -g bin -m 755 $$i.sh ${DESTDIR}/usr/games/$$i); done
	install -s -o games -g bin -m 700 ${HIDE} ${DESTDIR}/usr/games/hide
	install -s -o games -g bin -m 755 ${NOHIDE} ${DESTDIR}/usr/games
	-for i in ${HIDE}; do \
		(cd ${DESTDIR}/usr/games; rm -f $$i; ln -s dm $$i; chown games.bin $$i) \
	done

clean: FRC
	-for i in ${SUBDIR} games.vax games.tahoe; do \
		(cd $$i; make ${MFLAGS} clean); \
	done
	rm -f ${STD} ${NSTD} a.out core *.s *.o

depend: ldepend
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done

ldepend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

FRC:

# Files listed in ${NSTD} have explicit make lines given below.

primes:
	${CC} -o $@ ${CFLAGS} primes.c -lm

rain:
	${CC} -o $@ ${CFLAGS} rain.c -lcurses -ltermcap

worm:
	${CC} -o $@ ${CFLAGS} worm.c -lcurses -ltermcap

worms:
	${CC} -o $@ ${CFLAGS} worms.c -lcurses -ltermcap

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

arithmetic: arithmetic.c /usr/include/stdio.h /usr/include/signal.h
arithmetic: /usr/include/machine/trap.h
banner: banner.c /usr/include/stdio.h
bcd: bcd.c
factor: factor.c
fish: fish.c /usr/include/stdio.h
number: number.c /usr/include/stdio.h /usr/include/ctype.h
ppt: ppt.c /usr/include/stdio.h
primes: primes.c /usr/include/stdio.h /usr/include/math.h
rain: rain.c /usr/include/sys/types.h /usr/include/stdio.h /usr/include/sgtty.h
rain: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
rain: /usr/include/sys/ttydefaults.h /usr/include/sys/ttydev.h
rain: /usr/include/signal.h /usr/include/machine/trap.h
worm: worm.c /usr/include/ctype.h /usr/include/curses.h /usr/include/stdio.h
worm: /usr/include/sgtty.h /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
worm: /usr/include/sys/ttydefaults.h /usr/include/sys/ttydev.h
worm: /usr/include/signal.h /usr/include/machine/trap.h
worms: worms.c /usr/include/sys/types.h /usr/include/stdio.h
worms: /usr/include/sgtty.h /usr/include/sys/ioctl.h
worms: /usr/include/sys/ttychars.h /usr/include/sys/ttydefaults.h
worms: /usr/include/sys/ttydev.h /usr/include/signal.h
worms: /usr/include/machine/trap.h
wump: wump.c /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wump: /usr/include/sys/ttychars.h /usr/include/sys/ttydefaults.h
wump: /usr/include/sys/ttydev.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
