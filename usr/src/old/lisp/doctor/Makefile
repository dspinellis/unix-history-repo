#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.6	(Berkeley)	%G%
#
all: doctor

doctor: FRC
	liszt -mrq -o doctor doctor.l

install: FRC
	install -o bin -g bin -m 755 doctor ${DESTDIR}/usr/games/doctor

clean: FRC
	rm -f doctor core

depend: FRC
FRC:
