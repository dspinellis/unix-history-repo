#	@(#)Makefile	5.13 (Berkeley) %G%

PROG=	talk
DPADD=	${LIBCURSES} ${LIBTERMCAP} ${LIBCOMPAT}
LDADD=	-lcurses -ltermlib -lcompat
SRCS=	ctl.c ctl_transact.c display.c get_addrs.c get_names.c \
	init_disp.c invite.c io.c look_up.c msgs.c talk.c

.include <bsd.prog.mk>
