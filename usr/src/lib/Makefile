#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5 (Berkeley) 5/30/87
#
DESTDIR=
CFLAGS=	-O
TAGSFILE=tags

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	c2 cpp libc pcc

all:	${SUBDIR}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

FRC:

depend:
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} depend); \
	done

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); \
	done

tags:
	cd libc; make ${MFLAGS} TAGSFILE=../${TAGSFILE} tags

clean:
	rm -f a.out core *.s *.o
	-for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done
