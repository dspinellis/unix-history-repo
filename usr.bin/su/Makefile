#	$Id$
#	From: @(#)Makefile	5.5 (Berkeley) 5/11/90

PROG=	su
BINOWN=	root
BINMODE=4555

.if exists(${LIBCRYPT})
DPADD+= ${LIBCRYPT}
LDADD+= -lcrypt
.endif

.if exists(${LIBKRB})
DPADD+= ${LIBKRB} ${LIBDES}
LDADD+= -lkrb -ldes
CFLAGS+= -DKERBEROS
.endif

.include <bsd.prog.mk>
