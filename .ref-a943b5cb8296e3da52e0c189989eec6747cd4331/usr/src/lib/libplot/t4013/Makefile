#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
CFLAGS=	-O
OBJS=	arc.o box.o circle.o close.o dot.o erase.o label.o \
	line.o linemod.o move.o open.o point.o space.o subr.o

../lib4013:	${OBJS}
	ar cu ../lib4013 `lorder ${OBJS} | tsort`

clean:
	rm -f ${OBJS} errs a.out core
