#	@(#)Makefile	5.8 (Berkeley) %G%

PROG=	telnetd
CFLAGS+=-DNEED_GETTOS
SRCS=	get_date.c gettytab.c global.c slc.c state.c sys_term.c telnetd.c \
	termstat.c utility.c
DPADD=	${LIBUTIL} ${LIBTERM}
LDADD=	-lutil -ltermcap
MAN8=	telnetd.0
.PATH:	${.CURDIR}/../getty

.include <bsd.prog.mk>
