#	@(#)Makefile	8.3 (Berkeley) %G%

PROG=	mount_null
SRCS=	mount_null.c getmntopts.c
MAN8=	mount_null.0

MOUNT=	${.CURDIR}/../mount
CFLAGS+= -I/sys -I${MOUNT}
.PATH:	${MOUNT}

.include <bsd.prog.mk>
