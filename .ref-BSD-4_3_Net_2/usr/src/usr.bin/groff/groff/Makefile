#	@(#)Makefile	6.2 (Berkeley) 3/3/91

.include "../Makefile.g++"

PROG=		groff
SRCS=		groff.cc
LDADD+=		$(.CURDIR)/../libgroff/obj/libgroff.a
# Nuke -Ig++-include when we finally install libg++.
GXXFLAGS+=	-I$(.CURDIR)/../libgroff \
		-I$(.CURDIR)/../../../lib/libg++/g++-include

afterinstall:
	install -c -o $(BINOWN) -g $(BINGRP) -m $(BINMODE) \
		$(.CURDIR)/nroff.sh $(DESTDIR)$(BINDIR)/nroff
	install -c -o $(BINOWN) -g $(BINGRP) -m $(BINMODE) \
		$(.CURDIR)/psroff.sh $(DESTDIR)$(BINDIR)/psroff

.include "../../Makefile.inc"
.include <bsd.prog.mk>
