#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	arc.c box.c circle.c close.c cont.c dot.c erase.c label.c \
	line.c linemod.c move.c open.c point.c space.c subr.c
OBJS=	arc.o box.o circle.o close.o cont.o dot.o erase.o label.o \
	line.o linemod.o move.o open.o point.o space.o subr.o

all: ../libplotaed

../libplotaed: ${OBJS}
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
circle.o: circle.c aed.h /usr/include/stdio.h /usr/include/sgtty.h
circle.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
circle.o: /usr/include/sys/ttydev.h
close.o: close.c aed.h /usr/include/stdio.h /usr/include/sgtty.h
close.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
close.o: /usr/include/sys/ttydev.h
cont.o: cont.c aed.h /usr/include/stdio.h /usr/include/sgtty.h
cont.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
cont.o: /usr/include/sys/ttydev.h
dot.o: dot.c
erase.o: erase.c aed.h /usr/include/stdio.h /usr/include/sgtty.h
erase.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
erase.o: /usr/include/sys/ttydev.h
label.o: label.c aed.h /usr/include/stdio.h /usr/include/sgtty.h
label.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
label.o: /usr/include/sys/ttydev.h
line.o: line.c aed.h /usr/include/stdio.h /usr/include/sgtty.h
line.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
line.o: /usr/include/sys/ttydev.h
linemod.o: linemod.c aed.h /usr/include/stdio.h /usr/include/sgtty.h
linemod.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
linemod.o: /usr/include/sys/ttydev.h
move.o: move.c aed.h /usr/include/stdio.h /usr/include/sgtty.h
move.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
move.o: /usr/include/sys/ttydev.h
open.o: open.c aed.h /usr/include/stdio.h /usr/include/sgtty.h
open.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
open.o: /usr/include/sys/ttydev.h
point.o: point.c aed.h /usr/include/stdio.h /usr/include/sgtty.h
point.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
point.o: /usr/include/sys/ttydev.h
space.o: space.c aed.h /usr/include/stdio.h /usr/include/sgtty.h
space.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
space.o: /usr/include/sys/ttydev.h
subr.o: subr.c aed.h /usr/include/stdio.h /usr/include/sgtty.h
subr.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
subr.o: /usr/include/sys/ttydev.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
