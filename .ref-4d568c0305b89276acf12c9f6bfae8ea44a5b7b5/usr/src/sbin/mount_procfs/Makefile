#	@(#)Makefile	8.4 (Berkeley) %G%

PROG=	mount_procfs
SRCS=	mount_procfs.c getmntopts.c
MAN8=	mount_procfs.0

MOUNT=	${.CURDIR}/../mount
CFLAGS+= -I${MOUNT}
.PATH:	${MOUNT}

.include <bsd.prog.mk>
