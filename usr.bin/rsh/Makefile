#	$Id$
#	From: @(#)Makefile	5.6 (Berkeley) 9/27/90

PROG=	rsh
SRCS=	rsh.c krcmd.c
BINOWN=	root
BINMODE=4555
.PATH:	${.CURDIR}/../rlogin

.if exists(${DESTDIR}/usr/lib/libcrypt.a)
DPADD+= ${LIBCRYPT}
LDADD+= -lcrypt
.endif

.if exists(${DESTDIR}/usr/lib/libkrb.a)
DPADD+= ${LIBKRB} ${LIBDES}
LDADD+= -lkrb -ldes
CFLAGS+= -DKERBEROS -DCRYPT
.endif

.include <bsd.prog.mk>
