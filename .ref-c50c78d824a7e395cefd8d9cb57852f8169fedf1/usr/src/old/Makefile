#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.2	(Berkeley)	%G%
#
DESTDIR=
SUBDIR=	man compact

all: ${SUBDIR}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

install: FRC
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done

clean: FRC
	-for i in ${SUBDIR}; do (echo $$i; cd $$i; make ${MFLAGS} clean); done

depend:
	-for i in ${SUBDIR}; do (echo $$i; cd $$i; make ${MFLAGS} depend); done

FRC:
