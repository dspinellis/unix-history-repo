#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	make_key_perm
SRCS=	make_key_perm.c misc.c
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include \
	-I${.CURDIR}/../des
NOMAN=	noman

.include <bsd.prog.mk>
