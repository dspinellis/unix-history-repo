#	@(#)Makefile	4.14 (Berkeley) %G%

PROG=	sh
CFLAGS+=-w
SRCS=	setbrk.c builtin.c blok.c stak.c cmd.c fault.c main.c word.c \
	string.c name.c args.c xec.c service.c error.c io.c print.c \
	macro.c expand.c ctype.c msg.c
MAN1=	sh.0
DPADD=	${LIBCOMPAT}
LDADD=	-lcompat
MLINKS=	sh.1 case.1 sh.1 for.1 sh.1 if.1 sh.1 while.1

.include <bsd.prog.mk>
