#       @(#)Makefile    6.2 (Berkeley) 3/25/91

PROG=           gcc
SRCS=           gcc.c version.c obstack.c
MAN1=		gcc.1 g++.1
BINDIR=		/usr/bin
CFLAGS+=        -I$(.CURDIR) -I$(.CURDIR)/../common
.PATH:          $(.CURDIR)/../common 

afterinstall:
	install -c -o $(BINOWN) -g $(BINGRP) -m $(BINMODE) \
		$(.CURDIR)/g++.script $(DESTDIR)/usr/bin/g++

.include <bsd.prog.mk>
