#	@(#)Makefile	5.11.1.1 (Berkeley) 8/20/91

PROG=	passwd
SRCS=	krb_passwd.c local_passwd.c passwd.c pw_copy.c pw_util.c
CFLAGS+=-I${.CURDIR}
#SRCS=	des_rw.c krb_passwd.c local_passwd.c passwd.c pw_copy.c pw_util.c
#CFLAGS+=-I${.CURDIR} -DKERBEROS -DCRYPT
#DPADD=	${LIBKRB} ${LIBDES}
#LDADD=	-lkrb -ldes
.PATH:  ${.CURDIR}/../../usr.bin/chpass ${.CURDIR}/../../usr.sbin/vipw \
	${.CURDIR}/../rlogin
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
