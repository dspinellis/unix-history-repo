#	@(#)Makefile	5.11 (Berkeley) %G%

PROG=	telnetd
CFLAGS+=-DNEED_GETTOS -DLINEMODE -DKLUDGELINEMODE -DUSE_TERMIO -DDIAGNOSTICS
SRCS=	get_date.c gettytab.c global.c slc.c state.c sys_term.c telnetd.c \
	termstat.c utility.c
DPADD=	${LIBUTIL} ${LIBTERM}
LDADD=	-lutil -ltermcap
MAN8=	telnetd.0
.PATH:	${.CURDIR}/../getty

.include <bsd.prog.mk>
