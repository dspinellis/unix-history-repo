#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) 10/27/87
#
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	pi pc0 px pxp pdx utilities

all:
	for i in eyacc ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS}); \
	done

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

tags: FRC
	for i in eyacc ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} tags); \
	done

install: FRC
	for i in eyacc ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); \
	done

clean: FRC
	rm -f a.out core *.s *.o
	for i in eyacc ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} clean); \
	done

depend:
	(cd eyacc; make ${MFLAGS} depend)
	(cd eyacc; make ${MFLAGS} all)
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} depend); \
	done

FRC:
