#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
CFLAGS=	-O

OBJS = arc.o box.o circle.o clospl.o cont.o dot.o erase.o label.o \
	line.o linemd.o move.o open.o point.o space.o

../libf77plot:	${OBJS}
	ar cu ../libf77plot `lorder ${OBJS} | tsort`

clean:
	rm -f ${OBJS} errs a.out core
