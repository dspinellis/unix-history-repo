#	$Id: Makefile,v 1.5 1994/02/27 21:09:57 wollman Exp $
#	From: @(#)Makefile	5.5 (Berkeley) 5/11/90

PROG=	su
BINOWN=	root
BINMODE=4555

.if exists(${DESTDIR}/usr/lib/libcrypt.a)
DPADD+= ${LIBCRYPT}
LDADD+= -lcrypt
.endif

.if exists(${DESTDIR}/usr/lib/libkrb.a)
DPADD+= ${LIBKRB} ${LIBDES}
LDADD+= -lkrb -ldes
CFLAGS+= -DKERBEROS
.endif

.include <bsd.prog.mk>
