#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2	(Berkeley)	6/19/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	arc.c box.c circle.c close.c cont.c dot.c erase.c label.c \
	line.c linemod.c move.c open.c point.c space.c subr.c
OBJS=	arc.o box.o circle.o close.o cont.o dot.o erase.o label.o \
	line.o linemod.o move.o open.o point.o space.o subr.o

all: ../libplot2648

../libplot2648: ${OBJS}
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

arc.o: arc.c hp2648.h /usr/include/stdio.h
box.o: box.c
circle.o: circle.c hp2648.h /usr/include/stdio.h
close.o: close.c hp2648.h /usr/include/stdio.h
cont.o: cont.c hp2648.h /usr/include/stdio.h
dot.o: dot.c
erase.o: erase.c hp2648.h /usr/include/stdio.h
label.o: label.c hp2648.h /usr/include/stdio.h
line.o: line.c hp2648.h /usr/include/stdio.h
linemod.o: linemod.c hp2648.h /usr/include/stdio.h
move.o: move.c hp2648.h /usr/include/stdio.h
open.o: open.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
open.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h hp2648.h
open.o: /usr/include/stdio.h
point.o: point.c hp2648.h /usr/include/stdio.h
space.o: space.c hp2648.h /usr/include/stdio.h
subr.o: subr.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
subr.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h hp2648.h
subr.o: /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
