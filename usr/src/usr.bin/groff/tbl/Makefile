#	@(#)Makefile	6.1 (Berkeley) 3/3/91

.include "../Makefile.g++"
.include "../../Makefile.inc"

PROG=		tbl
SRCS=		main.cc table.cc
LDADD+=		$(.CURDIR)/../libgroff/obj/libgroff.a
# Delete the -Ig++-include after we fix libg++ and install it
GXXFLAGS+=	-I$(.CURDIR) -I$(.CURDIR)/../libgroff \
		-I$(.CURDIR)/../../../lib/libg++/g++-include

.include <bsd.prog.mk>
