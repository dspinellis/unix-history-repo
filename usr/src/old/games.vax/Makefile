#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	aard ching compat ddl rogue zork

# C sources that live in the current directory
#
SRCS=	chess.c

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	chess

all:	${SUBDIR} ${STD}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

FRC:

${STD}:
	${CC} ${CFLAGS} -o $@ $@.c

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${STD}; do \
		(install -s -o bin -g bin -m 755 $$i ${DESTDIR}/usr/games/$$i); \
	done
	install -c -o bin -g bin -m 444 chess.bin ${DESTDIR}/usr/games/lib/chess

clean:
	rm -f ${STD} a.out core *.s *.o
	-for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done

depend:
	-for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done
	mkdep ${CFLAGS} ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

chess.o: chess.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
