#
# Copyright (c) 1989 The Regents of the University of California.
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
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#
SUBDIR=	nroff troff nroff_term troff_font
FONTS=	fontlist ftB ftBC ftC ftCE ftCI ftCK ftCW ftFD ftG ftGI ftGM \
	ftGR ftH ftHI ftHM ftI ftI.bak ftL ftLI ftPA ftPB ftPI ftR \
	ftS ftSB ftSI ftSM ftTB ftTI ftTR ftTS ftUD ftap ftbb ftbi ftbk \
	ftbr ftcb ftch ftch.old ftci ftcl ftco ftcr ftct ftcy ftdb ftdi \
	ftdr ftfx ftgb ftgf ftgi ftgk ftgr fthb fthn ftm ftmb ftmi ftmn \
	ftmr ftnb ftni ftnr ftob ftoe ftoi ftor ftpb ftpp ftsb ftsc ftsg \
	ftsh ftsi ftsl ftsr fttB fttI fttR fttS fttb ftti fttr ftts ftwp \
	ftxx

all: FRC
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} all); done

clean cleandir depend lint tags: FRC
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} $@); done

install: FRC
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} $@); done
	cd fonts; install -c -o bin -g bin -m 444 ${FONTS} \
	    ${DESTDIR}/usr/libdata/troff_font

FRC:
