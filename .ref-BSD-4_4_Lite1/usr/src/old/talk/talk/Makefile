#	@(#)Makefile	5.1 (Berkeley) 5/31/90

PROG=	talk
CFLAGS+=-I${.CURDIR}/../common_source
SRCS=	ctl.c ctl_transact.c display.c get_addrs.c get_names.c init_disp.c \
	invite.c io.c look_up.c msgs.c talk.c
DPADD=	${LIBCURSES} ${LIBTERMCAP}
LDADD=	-lcurses -ltermcap
NOMAN=	noman

.include "../../Makefile.inc"
.include <bsd.prog.mk>
