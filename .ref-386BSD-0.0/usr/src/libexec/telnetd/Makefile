#	@(#)Makefile	5.16 (Berkeley) 5/13/91

PROG=	telnetd
CFLAGS+=-DLINEMODE -DKLUDGELINEMODE -DUSE_TERMIO -DDIAGNOSTICS
CFLAGS+=-I${.CURDIR}/../../lib
SRCS=	global.c slc.c state.c sys_term.c telnetd.c \
	termstat.c utility.c
DPADD=	${LIBUTIL} ${LIBTERM} ${LIBTELNET}
LDADD=	-lutil -ltermcap -ltelnet
MAN8=	telnetd.0

.include <bsd.prog.mk>
