#	@(#)Makefile	8.3 (Berkeley) %G%

PROG=	mount_cd9660
SRCS=	mount_cd9660.c getmntopts.c
MAN8=	mount_cd9660.0

MOUNT=	${.CURDIR}/../mount
CFLAGS+= -I${MOUNT}
.PATH:	${MOUNT}

.include <bsd.prog.mk>
