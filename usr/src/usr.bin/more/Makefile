#	@(#)Makefile	5.5 (Berkeley) %G%

PROG=	more
CFLAGS+=-I${.CURDIR}
SRCS=	ch.c command.c decode.c help.c input.c line.c linenum.c main.c \
	option.c os.c output.c position.c prim.c screen.c signal.c tags.c \
	ttyin.c
DPADD=	${LIBTERM}
LDADD=	-ltermcap

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/more.help \
	    ${DESTDIR}/usr/share/misc

.include <bsd.prog.mk>
