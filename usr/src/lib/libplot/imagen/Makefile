#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
CFLAGS=	-O
OBJS=	arc.o box.o circle.o close.o cont.o dot.o erase.o label.o line.o \
	linemod.o move.o open.o point.o scale.o space.o

all:	../libimagen

../libimagen: ${OBJS}
	ar cr ../libimagen ${OBJS}

close.o erase.o label.o line.o open.o: imp.h imPcodes.h
cont.o move.o:	imp.h

clean:
	rm -f *.o
