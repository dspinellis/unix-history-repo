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
SUBDIR=	eyacc pi pc0 px pxp pdx utilities

all:
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS}); \
	done

${SUBDIR}: /tmp
	cd $@; make ${MFLAGS}

install:
	for i in ${SUBDIR}; do \
		(echo $$i; cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); \
	done

clean:
	rm -f a.out core *.s *.o
	for i in ${SUBDIR}; do \
		(echo $$i; cd $$i; make ${MFLAGS} clean); \
	done
