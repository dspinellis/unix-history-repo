#	@(#)Makefile	8.3 (Berkeley) %G%

PROG=	umount
SRCS=	umount.c vfslist.c
DPADD=	${LIBRPC}
LDADD=	-lrpc

MOUNT=	${.CURDIR}/../../sbin/mount
CFLAGS+= -I${MOUNT}
.PATH:	${MOUNT}

.include <bsd.prog.mk>
