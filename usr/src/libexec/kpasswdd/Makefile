BINDIR=	/usr/libexec
PROG=	kpasswdd
SRCS=	kpasswdd.c des_rw.c
MAN8=	kpasswdd.0
DPADD=	${LIBKRB} ${LIBDES}
CFLAGS+=-DKERBEROS -I../kpasswd
LDADD=	-lkdb -lkrb -ldes
BINOWN=	bin
BINMODE=755
.PATH:  ${.CURDIR}/../../usr.bin/rlogin

.include <bsd.prog.mk>
