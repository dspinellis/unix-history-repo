#	@(#)Makefile	5.17 (Berkeley) %G%

PROG=	telnetd
CFLAGS+=-DLINEMODE -DKLUDGELINEMODE -DUSE_TERMIO -DDIAGNOSTICS
CFLAGS+=-DAUTHENTICATION -DENCRYPTION -I${.CURDIR}/../../lib
SRCS=	authenc.c global.c slc.c state.c sys_term.c telnetd.c \
	termstat.c utility.c
DPADD=	${LIBUTIL} ${LIBTERM}
LDADD=	-lutil -ltermcap -ltelnet
LDADD+=	-lkrb -ldes
MAN8=	telnetd.0

.include <bsd.prog.mk>
