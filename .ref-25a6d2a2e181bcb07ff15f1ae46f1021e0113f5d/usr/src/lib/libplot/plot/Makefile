#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	arc.c box.c circle.c close.c cont.c dot.c erase.c label.c \
	line.c linmod.c move.c open.c point.c putsi.c space.c
OBJS=	arc.o box.o circle.o close.o cont.o dot.o erase.o label.o \
	line.o linmod.o move.o open.o point.o putsi.o space.o

all: ../libplot

../libplot: ${OBJS}
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

arc.o: arc.c /usr/include/stdio.h
box.o: box.c
circle.o: circle.c /usr/include/stdio.h
close.o: close.c /usr/include/stdio.h
cont.o: cont.c /usr/include/stdio.h
dot.o: dot.c /usr/include/stdio.h
erase.o: erase.c /usr/include/stdio.h
label.o: label.c /usr/include/stdio.h
line.o: line.c /usr/include/stdio.h
linmod.o: linmod.c /usr/include/stdio.h
move.o: move.c /usr/include/stdio.h
open.o: open.c
point.o: point.c /usr/include/stdio.h
putsi.o: putsi.c /usr/include/stdio.h
space.o: space.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
