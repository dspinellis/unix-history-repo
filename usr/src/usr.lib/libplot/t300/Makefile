#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.4	(Berkeley)	6/19/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	arc.c box.c circle.c close.c dot.c erase.c label.c \
	line.c linmod.c move.c open.c point.c space.c subr.c
OBJS=	arc.o box.o circle.o close.o dot.o erase.o label.o \
	line.o linmod.o move.o open.o point.o space.o subr.o

all: ../lib300

../lib300: ${OBJS}
	ar cu $@ ${OBJS}

clean: FRC
	rm -f ${OBJS} core

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

install:
FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

arc.o: arc.c
box.o: box.c
circle.o: circle.c
close.o: close.c /usr/include/stdio.h
dot.o: dot.c
erase.o: erase.c con.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
erase.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
label.o: label.c con.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
label.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
line.o: line.c con.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
line.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
linmod.o: linmod.c
move.o: move.c
open.o: open.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
open.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
point.o: point.c
space.o: space.c con.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
space.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
subr.o: subr.c /usr/include/stdio.h con.h /usr/include/sgtty.h
subr.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
subr.o: /usr/include/sys/ttydev.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
