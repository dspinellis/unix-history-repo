#	From: @(#)Makefile	5.1 (Berkeley) 6/25/90
#	$Id$

PROG=	make_key_perm
SRCS=	make_key_perm.c misc.c
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include \
	-I${.CURDIR}/../des
NOMAN=	noman

# These things don't get installed
install:

.include <bsd.prog.mk>
