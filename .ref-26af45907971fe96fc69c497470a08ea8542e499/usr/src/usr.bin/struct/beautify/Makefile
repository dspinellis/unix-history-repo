#	@(#)Makefile	5.2 (Berkeley) %G%

PROG=	beautify
SRCS=	beauty.c tree.c lextab.c bdef.c
CFLAGS+=-w -I. -I${.CURDIR}
BINDIR=	/usr/libexec
DPADD=	${LIBLN}
LDADD=	-lln
NOMAN=	noman
CLEANFILES=beauty.c lextab.c y.tab.h lex.yy.c y.tab.c

.include <bsd.prog.mk>
