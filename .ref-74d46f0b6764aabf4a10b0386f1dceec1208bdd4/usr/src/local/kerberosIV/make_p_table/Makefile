#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	make_p_table
SRCS=	make_p_table.c misc.c
.PATH:	${.CURDIR}/../make_key_perm
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include \
	-I${.CURDIR}/../des
NOMAN=	noman

.include <bsd.prog.mk>
