#	$Id: Makefile,v 1.5 1994/03/04 20:33:56 wollman Exp $
#	From: @(#)Makefile	5.6 (Berkeley) 9/27/90

PROG=	rsh
SRCS=	rsh.c krcmd.c kcmd.c
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
