#	@(#)Makefile	5.11 (Berkeley) 2/19/91
#
# PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
# --------------------         -----   ----------------------
# CURRENT PATCH LEVEL:         1       50000
# --------------------         -----   ----------------------
#
# 10 Oct 92	Rodney W. Grimes	Install a real crypt for passwords
#

PROG=	passwd
SRCS=	local_passwd.c passwd.c pw_copy.c pw_util.c
.PATH:  ${.CURDIR}/../../usr.bin/chpass ${.CURDIR}/../../usr.sbin/vipw \
	${.CURDIR}/../rlogin
CFLAGS+=-I${.CURDIR} -DDES
BINOWN=	root
BINMODE=4555

.include <bsd.prog.mk>
