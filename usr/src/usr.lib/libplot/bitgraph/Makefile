#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2	(Berkeley)	6/19/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	box.c circle.c arc.c cont.c dot.c erase.c line.c linemod.c point.c \
	label.c move.c open.c close.c space.c
OBJS=	box.o circle.o arc.o cont.o dot.o erase.o line.o linemod.o point.o \
	label.o move.o open.o close.o space.o

all: ../libplotbg

../libplotbg: ${OBJS}
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

box.o: box.c
circle.o: circle.c
arc.o: arc.c bg.h /usr/include/stdio.h /usr/include/math.h
cont.o: cont.c bg.h /usr/include/stdio.h /usr/include/math.h
dot.o: dot.c
erase.o: erase.c bg.h /usr/include/stdio.h /usr/include/math.h
line.o: line.c bg.h /usr/include/stdio.h /usr/include/math.h
linemod.o: linemod.c
point.o: point.c
label.o: label.c
move.o: move.c bg.h /usr/include/stdio.h /usr/include/math.h
open.o: open.c /usr/include/signal.h /usr/include/machine/trap.h bg.h
open.o: /usr/include/stdio.h /usr/include/math.h
close.o: close.c /usr/include/signal.h /usr/include/machine/trap.h bg.h
close.o: /usr/include/stdio.h /usr/include/math.h
space.o: space.c bg.h /usr/include/stdio.h /usr/include/math.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
