#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=
OBJS=

all: libg.a

libg.a: dbxxx.s
	${AS} dbxxx.s -o $@

clean: FRC
	rm -f ${OBJS} core libg.a

install: FRC
	install -o bin -g bin -m 644 libg.a ${DESTDIR}/usr/lib/libg.a

depend:
lint:
tags:
FRC:
