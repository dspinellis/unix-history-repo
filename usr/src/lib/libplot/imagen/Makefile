#
# Copyright (c) 1985 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) %G%
#
CFLAGS=	-O
OBJS=	arc.o box.o charset.o circle.o close.o cont.o dot.o \
	erase.o label.o line.o linemod.o move.o open.o point.o \
	scale.o space.o
# default printer resolution dpi
DEFRES=	240

all:	../libplotimagen

../libplotimagen: ${OBJS}
	ar cr ../libplotimagen ${OBJS}

close.o erase.o label.o line.o open.o: imp.h imPcodes.h
cont.o move.o:	imp.h

space.o: space.c Makefile
	${CC} -c ${CFLAGS} -DDEFRES=${DEFRES} space.c

clean:
	rm -f ${OBJS} a.out core errs
