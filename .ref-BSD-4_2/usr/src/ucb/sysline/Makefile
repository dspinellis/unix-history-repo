# $Header: /usr/src/ucb/sysline/RCS/Makefile,v 1.2 83/02/10 02:15:06 layer Exp $
#  See the sysline program source for options you may want to modify
#
DESTDIR =

CFLAGS = -O -n

.DEFAULT:sysline

sysline: sysline.c
	${CC} ${CFLAGS} -o sysline sysline.c -lcurses -ltermlib

install: sysline
	install -s sysline $(DESTDIR)/usr/ucb

clean:
	rm -f sysline
