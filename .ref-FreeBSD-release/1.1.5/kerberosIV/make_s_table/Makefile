#	From: @(#)Makefile	5.1 (Berkeley) 6/25/90
#	$Id$

PROG=	make_s_table
SRCS=	make_s_table.c misc.c
.PATH:	${.CURDIR}/../make_key_perm
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include \
	-I${.CURDIR}/../des
NOMAN=	noman

# These things don't get installed
install:

.include <bsd.prog.mk>
