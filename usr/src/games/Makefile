#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.6 (Berkeley) 11/17/86
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	adventure backgammon battlestar boggle cribbage fortune \
	hangman hunt mille monop quiz robots sail snake trek

# Shell scripts that need only be installed and are never removed.
#
SCRIPT=	wargames

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	arithmetic btlgammon banner bcd cfscores factor fish number wump

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	canfield primes rain worm worms

# Programs that require a lisp interpreter or compiler
LISP=	doctor

all:	${SUBDIR} ${STD} ${NSTD}
	cd games.${MACHINE}; make ${MFLAGS}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

FRC:

${STD}:
	cc ${CFLAGS} -o $@ $@.c

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${SCRIPT}; do \
		(install -c $$i.sh ${DESTDIR}/usr/games/$$i); done
	-for i in ${STD} ${NSTD}; do \
		(install -s $$i ${DESTDIR}/usr/games/$$i); done
	-if [ ! -f ${DESTDIR}/usr/games/lib/cfscores ]; then \
		cat >${DESTDIR}/usr/games/lib/cfscores </dev/null; \
		chmod 777 ${DESTDIR}/usr/games/lib/cfscores; fi
	cd games.${MACHINE}; make ${MFLAGS} DESTDIR=${DESTDIR} install

clean:
	rm -f a.out core *.s *.o
	-for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done
	rm -f ${STD} ${NSTD}
	cd games.${MACHINE}; make ${MFLAGS} DESTDIR=${DESTDIR} clean

depend:
	for i in ${STD} ${NSTD}; do \
	    cc -M ${INCPATH} $$i.c | sed 's/\.o//' | \
	    awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
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

arithmetic: arithmetic.c /usr/include/stdio.h /usr/include/signal.h
btlgammon: btlgammon.c /usr/include/stdio.h
banner: banner.c /usr/include/stdio.h
bcd: bcd.c
cfscores: cfscores.c /usr/include/pwd.h
factor: factor.c
fish: fish.c /usr/include/stdio.h
number: number.c
wump: wump.c /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wump: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
canfield: canfield.c /usr/include/curses.h /usr/include/stdio.h
canfield: /usr/include/sgtty.h /usr/include/sys/ioctl.h
canfield: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
canfield: /usr/include/ctype.h /usr/include/signal.h /usr/include/sys/types.h
primes: primes.c /usr/include/stdio.h /usr/include/math.h
rain: rain.c /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
rain: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
rain: /usr/include/signal.h
worm: worm.c /usr/include/ctype.h /usr/include/curses.h /usr/include/stdio.h
worm: /usr/include/sgtty.h /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
worm: /usr/include/sys/ttydev.h /usr/include/signal.h
worms: worms.c /usr/include/stdio.h /usr/include/sgtty.h
worms: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
worms: /usr/include/sys/ttydev.h /usr/include/signal.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
