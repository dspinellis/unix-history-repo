#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.6	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	arc.c box.c circle.c close.c dot.c erase.c label.c \
	line.c linemod.c move.c open.c point.c space.c subr.c
OBJS=	arc.o box.o circle.o close.o dot.o erase.o label.o \
	line.o linemod.o move.o open.o point.o space.o subr.o

all: ../lib4014

../lib4014: ${OBJS}
	ar cu $@ `lorder ${OBJS} | tsort`

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
erase.o: erase.c
label.o: label.c
line.o: line.c
linemod.o: linemod.c
move.o: move.c
open.o: open.c
point.o: point.c
space.o: space.c
subr.o: subr.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
