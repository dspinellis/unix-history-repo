#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#
#  See the sysline program source for options you may want to modify
#
DESTDIR =

CFLAGS = -O

.DEFAULT:sysline

sysline: sysline.c
	${CC} ${CFLAGS} -o sysline sysline.c -ltermlib -lcurses

nsysline: sysline.c
	${CC} ${CFLAGS} -DTERMINFO -o nsysline sysline.c -lncurses

install: sysline
	install -s -g kmem -m 2755 sysline $(DESTDIR)/usr/ucb

clean:
	rm -f sysline

depend:
