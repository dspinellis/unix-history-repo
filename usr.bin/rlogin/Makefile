#	@(#)Makefile	5.6 (Berkeley) 9/27/90

PROG=	rlogin
SRCS=	rlogin.c
BINOWN=	root
BINMODE=4555

.if exists(${LIBCRYPT})
DPADD+= ${LIBCRYPT}
LDADD+= -lcrypt
.endif
.if exists(${LIBKRB})
DPADD+= ${LIBKRB} ${LIBDES}
LDADD+= -lkrb -ldes
CFLAGS+= -DKERBEROS -DCRYPT
.endif

.include <bsd.prog.mk>
