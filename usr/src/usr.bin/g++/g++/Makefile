#	@(#)Makefile	6.2 (Berkeley) 5/6/91

PROG=		g++
SRCS=		g++.c version.c obstack.c
CFLAGS+=	-I. -I$(.CURDIR)/../../gcc/cc1 \
		-I$(.CURDIR)/../../gcc/cc1/config \
		-DSTANDARD_EXEC_PREFIX=\"/usr/libexec/\"
CLEANFILES+=	g++.0
.PATH:		$(.CURDIR)/../../gcc/cc1

depend:

.include "../../Makefile.inc"
.include <bsd.prog.mk>
.include "../Makefile.symlinks"
