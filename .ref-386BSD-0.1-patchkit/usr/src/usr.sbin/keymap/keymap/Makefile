#	@(#)Makefile	5.4 (Berkeley) 6/5/91
# Made it also look in lib/obj for the library.. rgrimes 93/05/26
# this is still not right but it gets me past build world for now...

PROG=		keymap
SRCS=		keymap.c
BINDIR=         /usr/bin

DPADD=		${.CURDIR}/../lib/libconsutil.a
LDADD=		-lconsutil
LDFLAGS+=	-L${.CURDIR}/../lib
LDFLAGS+=	-L${.CURDIR}/../lib/obj

MAN5=		keycap.0
MAN8=		keymap.0

.include 	<bsd.prog.mk>
