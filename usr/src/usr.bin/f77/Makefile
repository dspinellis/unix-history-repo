#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#
DESTDIR	      =

SUBDIR	      = f77.${MACHINE} fpr fsplit

all:		${SUBDIR}

${SUBDIR}:	FRC
		cd $@; make ${MFLAGS}

FRC:

install:
		for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done

clean:
		for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done

depend:
		for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done
