#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	telnet
SRCS=	authenc.c commands.c main.c network.c ring.c sys_bsd.c telnet.c \
	terminal.c tn3270.c utilities.c
CFLAGS+=-DLETS_GET_SMALL -DTERMCAP -DKLUDGELINEMODE -DUSE_TERMIO \
	-DAUTHENTICATE -DENCRYPT -DKRB4 -I${.CURDIR}/../../../../lib
LDADD=  -ltermcap -ltelnet -lkrb -ldes
DPADD=  ${LIBTERMCAP} ${LIBTELNET} ${LIBKRB} ${LIBDES}

NOMAN=	noman
LDFLAGS=-N
.PATH:	${.CURDIR}/../../../../usr.bin/telnet

.include <bsd.prog.mk>
