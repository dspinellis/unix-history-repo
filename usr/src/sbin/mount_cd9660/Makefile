#	@(#)Makefile	8.2 (Berkeley) %G%

PROG=	mount_cd9660
SRCS=	mount_cd9660.c getmntopts.c
MAN8=	${PROG}.0
MOUNT=	${.CURDIR}/../mount
CFLAGS+= -I${MOUNT}
.PATH:	${MOUNT}

.include <bsd.prog.mk>
