#	@(#)Makefile	5.1 (Berkeley) 5/3/91

DPADD=	${LIBTERM}
LDADD=	-ltermcap

PROG=	elvis
SRCS=	blk.c cmd1.c cmd2.c curses.c cut.c ex.c input.c main.c misc.c \
	modify.c move1.c move2.c move3.c move4.c move5.c opts.c recycle.c \
	redraw.c regexp.c regsub.c system.c tio.c tmp.c vars.c vcmd.c vi.c

CFLAGS+=-DLETS_GET_SMALL
NOMAN=	noman
LDFLAGS=-s -N
.PATH:	${.CURDIR}/../../../../contrib/elvis

.include <bsd.prog.mk>
