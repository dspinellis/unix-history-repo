#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.17 (Berkeley) %G%
#
#	Makefile	4.15	84/12/03
#
DESTDIR=
CFLAGS=	-O

# The following sources are incomplete and cannot be compiled without
# some work
WORK=	aard ddl mpu

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	adventure backgammon battlestar boggle ching compat cribbage doctor \
	fortune hangman hunt mille monop quiz robots rogue sail snake trek zork

# Shell scripts that need only be installed and are never removed.
#
SCRIPT=	wargames

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	arithmetic btlgammon banner bcd cfscores chess factor \
	fish number wump

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	canfield primes rain worm worms

all:	${SUBDIR} ${STD} ${NSTD}

${SUBDIR}: /tmp
	cd $@; make ${MFLAGS}

${STD}:
	cc ${CFLAGS} -o $@ $@.c

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${SCRIPT}; do \
		(install -c $$i.sh ${DESTDIR}/usr/games/$$i); done
	-for i in ${STD} ${NSTD}; do \
		(install -s $$i ${DESTDIR}/usr/games/$$i); done
	install -c chess.bin ${DESTDIR}/usr/games/lib/chess
	cat >${DESTDIR}/usr/games/lib/cfscores </dev/null
	chmod 777 ${DESTDIR}/usr/games/lib/cfscores

clean:
	rm -f a.out core *.s *.o
	-for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done
	rm -f ${STD} ${NSTD}

depend:
	cat </dev/null >x.c
	for i in ${STD} ${NSTD}; do \
		(echo $$i: $$i.c >>makedep; \
		/bin/grep '^#[ 	]*include' x.c $$i.c | sed \
			-e 's,<\(.*\)>,"/usr/include/\1",' \
			-e 's/:[^"]*"\([^"]*\)".*/: \1/' \
			-e 's/\.c//' >>makedep); done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep x.c
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# Files listed in ${NSTD} have explicit make lines given below.

canfield:
	cc -o canfield ${CFLAGS} canfield.c -lcurses -ltermcap

primes:
	cc -o primes ${CFLAGS} primes.c -lm

rain:
	cc -o rain ${CFLAGS} rain.c -lcurses -ltermcap

worm:
	cc -o worm ${CFLAGS} worm.c -lcurses -ltermcap

worms:
	cc -o worms ${CFLAGS} worms.c -lcurses -ltermcap

# DO NOT DELETE THIS LINE -- make depend uses it

arithmetic: arithmetic.c
arithmetic: /usr/include/stdio.h
arithmetic: /usr/include/signal.h
btlgammon: btlgammon.c
btlgammon: /usr/include/stdio.h
banner: banner.c
banner: /usr/include/stdio.h
bcd: bcd.c
cfscores: cfscores.c
cfscores: /usr/include/pwd.h
chess: chess.c
chess: /usr/include/stdio.h
fish: fish.c
fish: /usr/include/stdio.h
number: number.c
wump: wump.c
wump: /usr/include/stdio.h
wump: /usr/include/sgtty.h
canfield: canfield.c
canfield: /usr/include/curses.h
canfield: /usr/include/ctype.h
canfield: /usr/include/signal.h
canfield: /usr/include/sys/types.h
rain: rain.c
rain: /usr/include/stdio.h
rain: /usr/include/sgtty.h
rain: /usr/include/signal.h
worm: worm.c
worm: /usr/include/ctype.h
worm: /usr/include/curses.h
worm: /usr/include/signal.h
worms: worms.c
worms: /usr/include/stdio.h
worms: /usr/include/sgtty.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
