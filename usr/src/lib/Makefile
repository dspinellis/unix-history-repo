#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.12 (Berkeley) %G%
#
DESTDIR=
CFLAGS=	-O
LIBC=	/lib/libc.a
TAGSFILE=tags

# Subdirectories whose routines are included in the making of the
# master tags file (/usr/lib/tags); the Fortran libraries should
# be on this list, but we don't control them....
#
TAGSDIR=libcurses libdbm libln libm libmp libpc libtermlib

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	learn lib2648 libF77 libI77 libU77 libcurses libdbm libg libln \
	libom libm libmp libpc libplot libtermlib liby lpr me sendmail

# C source that lives in the current directory
#
SRCS=	getNAME.c makekey.c

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	getNAME makekey

# C programs that live in the current directory and need explicit make lines.
#
NSTD=

all: ${SUBDIR} ${STD} ${NSTD}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

${STD}: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

install: FRC
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); \
	done
	-for i in ${STD} ${NSTD}; do \
		(install -s -o bin -g bin -m 755 $$i ${DESTDIR}/usr/lib/$$i); \
	done
	install -c -o bin -g bin -m 755 lib.b ${DESTDIR}/usr/lib/lib.b

tags: FRC
	-for i in ${TAGSDIR}; do \
		(cd $$i; make ${MFLAGS} TAGSFILE=../${TAGSFILE} tags); \
	done

clean: FRC
	rm -f a.out core *.s *.o tags ${STD} ${NSTD}
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done

depend: ldepend
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done

ldepend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

FRC:

# Files listed in ${NSTD} have explicit make lines given below.

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

getNAME: getNAME.c /usr/include/strings.h /usr/include/stdio.h
makekey: makekey.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
