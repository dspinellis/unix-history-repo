#	@(#)Makefile	8.1 (Berkeley) 6/6/93

PROG=	uucico
SRCS=	chksum.c cico.c cntrl.c condevs.c conn.c fio.c gio.c gnsys.c \
	imsg.c pk0.c pk1.c tio.c
CFLAGS+=-I${.CURDIR}/../includes
BINDIR=	${LIBDIR}
BINMODE=6555
DPADD=	${LIBCOMPAT}
LDADD=	${LIBUU} ${LIBACU} -lcompat
MAN8=	uucico.0

.include <bsd.prog.mk>
