#	@(#)Makefile	6.4 (Berkeley) 2/21/91

PROG=		cpp
BINDIR=		/usr/libexec
SRCS=		cccp.c cexp.y version.c
CFLAGS+=	-g -I. -I$(.CURDIR) -I$(.CURDIR)/../common \
		-DGCC_INCLUDE_DIR=\"\" \
		-DGPLUSPLUS_INCLUDE_DIR=\"/usr/include/g++\"
YFLAGS=
.PATH:		$(.CURDIR)/../common

afterinstall:
	install -c -o $(BINOWN) -g $(BINGRP) -m $(BINMODE) \
		$(.CURDIR)/usr.bin.cpp.sh $(DESTDIR)/usr/bin/cpp

.include <bsd.prog.mk>
