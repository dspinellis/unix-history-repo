#
# @(#)Makefile	1.5	(Berkeley) 2/8/83
#
DESTDIR = /usr/ucb

CFLAGS = -O -n -DVMUNIX -DHOSTNAME

.DEFAULT:sysline

sysline: sysline.c
	${CC} ${CFLAGS} -o sysline sysline.c -lcurses -ltermlib

install: sysline
	install -c -s sysline $(DESTDIR)

clean:
	rm -f sysline
