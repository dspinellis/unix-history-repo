#	@(#)Makefile	5.6 (Berkeley) 9/27/90
#
# PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
# --------------------         -----   ----------------------
# CURRENT PATCH LEVEL:         1       50000
# --------------------         -----   ----------------------
#
# 10 Oct 92	Rodney W. Grimes	Install a real crypt for passwords
#

CFLAGS+=-DDES
PROG=	rlogin
SRCS=	rlogin.c
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
