#	@(#)Makefile	8.4 (Berkeley) %G%

PROG=	umount
SRCS=	umount.c vfslist.c
MAN8=	umount.0
DPADD=	${LIBRPC}
LDADD=	-lrpc

MOUNT=	${.CURDIR}/../../sbin/mount
CFLAGS+= -I${MOUNT}
.PATH:	${MOUNT}

.include <bsd.prog.mk>
