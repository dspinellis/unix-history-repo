#	@(#)Makefile	8.1 (Berkeley) 6/1/93

PROG=	make_p_table
SRCS=	make_p_table.c misc.c
.PATH:	${.CURDIR}/../make_key_perm
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include \
	-I${.CURDIR}/../des
NOMAN=	noman

.include <bsd.prog.mk>
