#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5	(Berkeley)	6/19/87
#
# DEFRES == default printer resolution dpi
CFLAGS=	-O -DDEFRES=240
LIBC=	/lib/libc.a
SRCS=	arc.c box.c charset.c circle.c close.c cont.c dot.c \
	erase.c label.c line.c linemod.c move.c open.c point.c \
	scale.c space.c
OBJS=	arc.o box.o charset.o circle.o close.o cont.o dot.o \
	erase.o label.o line.o linemod.o move.o open.o point.o \
	scale.o space.o

all: ../libplotimagen

../libplotimagen: ${OBJS}
	ar cr $@ ${OBJS}

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
charset.o: charset.c imp.h /usr/include/stdio.h
circle.o: circle.c
close.o: close.c imp.h /usr/include/stdio.h imPcodes.h
cont.o: cont.c /usr/include/stdio.h imp.h /usr/include/stdio.h
dot.o: dot.c
erase.o: erase.c imPcodes.h imp.h /usr/include/stdio.h
label.o: label.c imPcodes.h imp.h /usr/include/stdio.h
line.o: line.c imp.h /usr/include/stdio.h imPcodes.h
linemod.o: linemod.c imp.h /usr/include/stdio.h
move.o: move.c imp.h /usr/include/stdio.h
open.o: open.c imPcodes.h imp.h /usr/include/stdio.h
point.o: point.c
scale.o: scale.c
space.o: space.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
