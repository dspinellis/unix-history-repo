#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
ALL=	../protocol.h query.c trace.c
CFLAGS=	-O
DESTDIR=
LDFLAGS=

all:	query trace

# test programs and utilities
query:	query.o
	${CC} -o query ${LDFLAGS} query.o

trace:	trace.o
	${CC} -o trace ${LDFLAGS} trace.o

install:
	@echo Nothing to install

clean:
	rm -f *.o core a.out t.c query linterrs trace typescript errs
