#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	arc.c box.c circle.c clospl.c cont.c dot.c erase.c label.c \
	line.c linemd.c move.c open.c point.c space.c
OBJS=	arc.o box.o circle.o clospl.o cont.o dot.o erase.o label.o \
	line.o linemd.o move.o open.o point.o space.o

all: ../libf77plot

../libf77plot: ${OBJS}
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
clospl.o: clospl.c
cont.o: cont.c
dot.o: dot.c
erase.o: erase.c
label.o: label.c
line.o: line.c
linemd.o: linemd.c
move.o: move.c
open.o: open.c
point.o: point.c
space.o: space.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
