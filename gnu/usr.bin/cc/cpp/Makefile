#	@(#)Makefile	6.4 (Berkeley) 2/21/91

PROG=		gcpp
BINDIR=		/usr/libexec
SRCS=		cccp.c cexp.y version.c
MAN1=		gcpp.1
CFLAGS+=	-I. -I$(.CURDIR) -I$(.CURDIR)/../common \
		-DGCC_INCLUDE_DIR=\"\" \
		-DGPLUSPLUS_INCLUDE_DIR=\"/usr/include/g++\"
YFLAGS=
.PATH:		$(.CURDIR)/../common

.include <bsd.prog.mk>
