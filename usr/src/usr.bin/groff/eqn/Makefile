#	@(#)Makefile	6.1 (Berkeley) 3/3/91

.include "../Makefile.g++"
.include "../../Makefile.inc"

PROG=		eqn
SRCS=		box.cc delim.cc eqn.y lex.cc limit.cc list.cc \
		main.cc mark.cc other.cc over.cc pile.cc script.cc \
		sqrt.cc text.cc
LDADD+=		$(.CURDIR)/../libgroff/obj/libgroff.a
# Delete the -Ig++-include after we fix libg++ and install it
GXXFLAGS+=	-I. -I$(.CURDIR) -I$(.CURDIR)/../libgroff \
		-I$(.CURDIR)/../../../lib/libg++/g++-include
CLEANFILES+=	eqn.tab.h

.include <bsd.prog.mk>
