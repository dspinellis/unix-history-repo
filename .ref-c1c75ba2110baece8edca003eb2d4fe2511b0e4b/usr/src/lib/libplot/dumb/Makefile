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

all: ../libplotdumb

../libplotdumb: ${OBJS}
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

arc.o: arc.c dumb.h /usr/include/math.h
box.o: box.c
circle.o: circle.c
close.o: close.c /usr/include/signal.h dumb.h /usr/include/math.h
cont.o: cont.c dumb.h /usr/include/math.h
dot.o: dot.c
erase.o: erase.c dumb.h /usr/include/math.h
label.o: label.c dumb.h /usr/include/math.h
line.o: line.c dumb.h /usr/include/math.h
linemod.o: linemod.c
move.o: move.c dumb.h /usr/include/math.h
open.o: open.c /usr/include/signal.h dumb.h /usr/include/math.h
point.o: point.c dumb.h /usr/include/math.h
space.o: space.c dumb.h /usr/include/math.h
subr.o: subr.c dumb.h /usr/include/math.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
