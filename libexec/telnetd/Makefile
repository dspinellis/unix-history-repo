#	@(#)Makefile	5.16 (Berkeley) 5/13/91
#
# PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
# --------------------         -----   ----------------------
# CURRENT PATCH LEVEL:         1       00035
# --------------------         -----   ----------------------
#
# 27 Nov 1992	Felix Gaehtgens		Removed KLUDGELINEMODE
#
PROG=	telnetd
CFLAGS+=-DLINEMODE -DUSE_TERMIO -DDIAGNOSTICS
CFLAGS+=-I${.CURDIR}/../../lib
SRCS=	global.c slc.c state.c sys_term.c telnetd.c \
	termstat.c utility.c
DPADD=	${LIBUTIL} ${LIBTERM} ${LIBTELNET}
LDADD=	-lutil -ltermcap -ltelnet
MAN8=	telnetd.0

.include <bsd.prog.mk>
