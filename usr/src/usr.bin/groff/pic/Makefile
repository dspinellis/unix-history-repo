#	@(#)Makefile	6.1 (Berkeley) 3/3/91

.include "../Makefile.g++"
.include "../../Makefile.inc"

PROG=		pic
SRCS=           common.cc main.cc object.cc output.cc \
		pic.y key.cc lex.cc tex.cc troff.cc
LDADD+=		$(.CURDIR)/../libgroff/obj/libgroff.a
# Delete the -Ig++-include after we fix libg++ and install it
GXXFLAGS+=	-I. -I$(.CURDIR) -I$(.CURDIR)/../libgroff \
		-I$(.CURDIR)/../../../lib/libg++/g++-include
CLEANFILES+=	pic.tab.h

.include <bsd.prog.mk>
