#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5 (Berkeley) %G%
#
DESTDIR=
CC = /bin/cc
CFLAGS = -O

all:	boggle bogdict

bogdict: comp sfile
	-if [ -f /usr/local/lib/w2a ]; \
		then dict=/usr/local/lib/w2a; \
		else dict=/usr/dict/words; fi; \
	sed -f sfile $$dict | uniq | ./comp >bogdict

install:
	install -s boggle ${DESTDIR}/usr/games/boggle
	install bogdict ${DESTDIR}/usr/games/bogdict

boggle: boggle.c
	cc -o boggle ${CFLAGS} boggle.c

comp: comp.c
	cc -o comp ${CFLAGS} comp.c

clean:
	rm -f bogdict boggle comp

depend:
