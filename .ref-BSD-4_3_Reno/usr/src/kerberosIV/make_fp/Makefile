#	@(#)Makefile	5.1 (Berkeley) 6/25/90

PROG=	make_fp
SRCS=	make_fp.c misc.c
.PATH:	${.CURDIR}/../make_key_perm
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include \
	-I${.CURDIR}/../des
NOMAN=	noman

.include <bsd.prog.mk>
