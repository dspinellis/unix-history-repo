#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.3	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	tmac.an tmac.an.new tmac.an6n tmac.an6t

all:

install: FRC
	for i in ${SRCS}; do \
		(install -c -o bin -g bin -m 644 $$i ${DESTDIR}/usr/lib/tmac/$$i); \
	done

clean:
depend:
FRC:
