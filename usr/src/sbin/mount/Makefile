#	@(#)Makefile	8.1 (Berkeley) 6/5/93

PROG=	mount
CFLAGS+=-DNFS
MAN8=	mount.0
DPADD=	${LIBRPC}
LDADD=	-lrpc
MLINKS=	mount.8 umount.8

.include <bsd.prog.mk>
