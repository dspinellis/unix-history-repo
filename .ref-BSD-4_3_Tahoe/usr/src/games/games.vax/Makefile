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
#	@(#)Makefile	5.4 (Berkeley) 7/9/88
#
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	aard ching compat ddl zork

# C sources that live in the current directory
#
SRCS=	chess.c

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	chess

all: ${SUBDIR} ${STD}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

${STD}:
	${CC} ${CFLAGS} -o $@ $@.c

install: FRC
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${STD}; do \
		(install -s -o bin -g bin -m 755 $$i ${DESTDIR}/usr/games/$$i); \
	done
	install -c -o bin -g bin -m 444 chess.bin ${DESTDIR}/usr/games/lib/chess

clean: FRC
	-for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done
	rm -f ${STD} a.out core *.s *.o

depend: FRC
	-for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done
	mkdep ${CFLAGS} ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

chess.o: chess.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
