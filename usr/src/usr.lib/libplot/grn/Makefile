#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.2	(Berkeley)	6/19/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	arc.c box.c circle.c close.c cont.c dot.c erase.c label.c \
	line.c linemod.c move.c open.c point.c space.c subr.c
OBJS=	arc.o box.o circle.o close.o cont.o dot.o erase.o label.o \
	line.o linemod.o move.o open.o point.o space.o subr.o

all: ../libplotgrn

../libplotgrn: ${OBJS}
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

arc.o: arc.c grnplot.h /usr/include/stdio.h
box.o: box.c
circle.o: circle.c grnplot.h /usr/include/stdio.h
close.o: close.c grnplot.h /usr/include/stdio.h
cont.o: cont.c grnplot.h /usr/include/stdio.h
dot.o: dot.c
erase.o: erase.c grnplot.h /usr/include/stdio.h
label.o: label.c grnplot.h /usr/include/stdio.h
line.o: line.c grnplot.h /usr/include/stdio.h
linemod.o: linemod.c grnplot.h /usr/include/stdio.h
move.o: move.c grnplot.h /usr/include/stdio.h
open.o: open.c grnplot.h /usr/include/stdio.h
point.o: point.c grnplot.h /usr/include/stdio.h
space.o: space.c grnplot.h /usr/include/stdio.h
subr.o: subr.c grnplot.h /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
