#	@(#)Makefile	5.15 (Berkeley) 7/1/90
#
# PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
# --------------------         -----   ----------------------
# CURRENT PATCH LEVEL:         1       50000
# --------------------         -----   ----------------------
#
# 10 Oct 92	Rodney W. Grimes	Install a real crypt for passwords
#

PROG=	ftpd
CFLAGS+=-I${.CURDIR}/../../usr.bin/ftp -DSETPROCTITLE -DDES
SRCS=	ftpd.c ftpcmd.c glob.c logwtmp.c popen.c vers.c
MAN8=	ftpd.8
CLEANFILES+=ftpcmd.c y.tab.h
.PATH:	${.CURDIR}/../../usr.bin/ftp

.include <bsd.prog.mk>
