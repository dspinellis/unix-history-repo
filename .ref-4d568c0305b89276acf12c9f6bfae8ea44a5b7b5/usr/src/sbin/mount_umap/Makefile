#	@(#)Makefile	8.3 (Berkeley) %G%

PROG=	mount_umap
SRCS=	mount_umap.c getmntopts.c
MAN8=	mount_umap.0

MOUNT=	${.CURDIR}/../mount
CFLAGS+= -I/sys -I${MOUNT}
.PATH:	${MOUNT}

.include <bsd.prog.mk>
