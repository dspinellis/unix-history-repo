#	@(#)Makefile	5.5 (Berkeley) 5/11/90

PROG=	su
CFLAGS+=-DDES
BINOWN=	root
BINMODE=4555

.if exists(/usr/lib/libcrypt.a)
#CFLAGS+=-DKERBEROS
DPADD+= ${LIBCRYPT}
LDADD+= -lcrypt
.endif

.include <bsd.prog.mk>
