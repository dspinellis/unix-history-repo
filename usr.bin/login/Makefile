#	@(#)Makefile	5.6 (Berkeley) 6/24/90
#
# PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
# --------------------         -----   ----------------------
# CURRENT PATCH LEVEL:         1       50000
# --------------------         -----   ----------------------
#
# 10 Oct 92	Rodney W. Grimes	Install a real crypt for passwords
#

CFLAGS+=-DDES
PROG=	login
SRCS=	login.c
DPADD=	${LIBUTIL}
LDADD=	-lutil
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
