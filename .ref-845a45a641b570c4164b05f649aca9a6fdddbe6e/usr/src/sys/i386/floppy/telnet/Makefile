#	@(#)Makefile	5.2 (Berkeley) %G%

PROG=	telnet
SRCS=	authenc.c commands.c main.c network.c ring.c sys_bsd.c telnet.c \
	terminal.c utilities.c
CFLAGS+=-s -N -DLETS_GET_SMALL -DTERMCAP -DUSE_TERMIO \
	-I${.CURDIR}/../../../../lib
LDADD=  -ltelnet -ltermcap
DPADD=  ${LIBTELNET} ${LIBTERMCAP}

NOMAN=	noman
.PATH:	${.CURDIR}/../../../../usr.bin/telnet

.include <bsd.prog.mk>
