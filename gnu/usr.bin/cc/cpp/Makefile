#	From: @(#)Makefile	6.4 (Berkeley) 2/21/91
#	$Id$

PROG=		gcpp
BINDIR=		/usr/libexec
SRCS=		cccp.c cexp.y version.c
MAN1=		gcpp.1
CFLAGS+=	-I. -I$(.CURDIR) -I$(.CURDIR)/../lib \
		-DGCC_INCLUDE_DIR=\"\" \
		-DGPLUSPLUS_INCLUDE_DIR=\"/usr/include/g++\"
YFLAGS=

MLINKS=	gcpp.1 cpp.1

.PATH:		$(.CURDIR)/../lib

afterinstall:
	install -c -o $(BINOWN) -g $(BINGRP) -m $(BINMODE) \
		$(.CURDIR)/usr.bin.cpp.sh $(DESTDIR)/usr/bin/gcpp
	@/bin/rm -rf $(DESTDIR)/usr/bin/cpp
	@ln -s $(DESTDIR)/usr/bin/gcpp $(DESTDIR)/usr/bin/cpp
	@echo $(DESTDIR)/usr/bin/cpp -\> $(DESTDIR)/usr/bin/gcpp

.include <bsd.prog.mk>
