#	@(#)Makefile	6.1 (Berkeley) 3/3/91

.include "../Makefile.g++"

PROG=		grops
SRCS=		ps.cc
LDADD+=		$(.CURDIR)/../libdriver/obj/libdriver.a \
		$(.CURDIR)/../libgroff/obj/libgroff.a
# Nuke -Ig++-include when we finally install libg++.
GXXFLAGS+=	-I$(.CURDIR)/../libdriver -I$(.CURDIR)/../libgroff \
		-I$(.CURDIR)/../../../lib/libg++/g++-include

afterinstall:
	echo install macros and devices

.include "../../Makefile.inc"
.include <bsd.prog.mk>
