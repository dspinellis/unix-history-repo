#	@(#)Makefile	5.12 (Berkeley) %G%

PROG=	talk
DPADD=	${LIBCURSES} ${LIBTERMCAP} ${LIBCOMPAT}
LDADD=	-lcurses -ltermlib -lcompat
SRCS=	talk.c get_names.c display.c io.c ctl.c init_disp.c \
	msgs.c get_addrs.c ctl_transact.c invite.c look_up.c

.include <bsd.prog.mk>
