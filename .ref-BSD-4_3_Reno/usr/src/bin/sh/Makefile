#	@(#)Makefile	4.12 (Berkeley) 5/11/90

PROG=	sh
CFLAGS+=-w
SRCS=	setbrk.c builtin.c blok.c stak.c cmd.c fault.c main.c word.c \
	string.c name.c args.c xec.c service.c error.c io.c print.c \
	macro.c expand.c ctype.c msg.c
MAN1=	cd.0 intro.0 sh.0 wait.0
DPADD=	${LIBCOMPAT}
LDADD=	-lcompat
MLINKS=	sh.1 case.1 sh.1 for.1 sh.1 if.1 sh.1 while.1

.include <bsd.prog.mk>
