#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.7	(Berkeley)	9/19/87
#
all: doctor

doctor: FRC
	liszt -mrq -o doctor doctor.l

install: FRC
	install -o games -g bin -m 700 doctor ${DESTDIR}/usr/games/hide
	rm -f ${DESTDIR}/usr/games/doctor
	ln -s ${DESTDIR}/usr/games/dm ${DESTDIR}/usr/games/doctor
	chown games.bin ${DESTDIR}/usr/games/doctor

clean: FRC
	rm -f doctor core

depend: FRC
FRC:
