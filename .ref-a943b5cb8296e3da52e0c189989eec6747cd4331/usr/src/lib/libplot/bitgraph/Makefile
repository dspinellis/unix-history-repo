# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
#
CFLAGS=	-O
OBJS=	box.o circle.o arc.o cont.o dot.o erase.o line.o linemod.o point.o \
	label.o move.o open.o close.o space.o

../libplotbg:	${OBJS}
	ar cu ../libplotbg ${OBJS}

${OBJS}:	bg.h

clean:
	rm -f ${OBJS} errs a.out core
