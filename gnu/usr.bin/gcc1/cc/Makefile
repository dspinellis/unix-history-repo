#	@(#)Makefile	6.2 (Berkeley) 3/25/91

PROG=		cc
SRCS=		cc.c version.c obstack.c
CFLAGS+=	-I. -I$(.CURDIR)/../cc1 -I$(.CURDIR)/../cc1/config
.PATH:		$(.CURDIR)/../cc1

.include "../../Makefile.inc"

.include <bsd.prog.mk>
.include "../Makefile.symlinks"
