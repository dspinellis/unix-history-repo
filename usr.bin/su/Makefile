#	$Id: Makefile,v 1.6 1994/03/04 20:38:03 wollman Exp $
#	From: @(#)Makefile	5.5 (Berkeley) 5/11/90

CFLAGS += -g

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
