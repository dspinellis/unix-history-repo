#       @(#)Makefile    6.2 (Berkeley) 3/25/91

PROG=           gcc
SRCS=           gcc.c version.c obstack.c
MAN1=		gcc.0 g++.0
BINDIR=		/usr/bin
CFLAGS+=        -I$(.CURDIR) -I$(.CURDIR)/../common
.PATH:          $(.CURDIR)/../common 


afterinstall:
	-rm -f $(DESTDIR)$(BINDIR)/gcc
	cd $(DESTDIR)$(BINDIR); ln -s cc gcc

.include <bsd.prog.mk>
