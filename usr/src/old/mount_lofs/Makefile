#	@(#)Makefile	8.3 (Berkeley) %G%

PROG=	mount_lofs
SRCS=	mount_lofs.c getmntopts.c
MAN8=	mount_lofs.0

MOUNT=	${.CURDIR}/../mount
CFLAGS+=-I/sys -I${MOUNT}
.PATH:	${MOUNT}

.include <bsd.prog.mk>
