#	@(#)Makefile	5.8 (Berkeley) %G%

LIB=	util
CFLAGS+=-DLIBC_SCCS -I/sys
SRCS=	daemon.c getloadavg.c login.c login_tty.c logout.c logwtmp.c kvm.c \
	pty.c pwcache.c
MAN3=	getloadavg.0 pwcache.0

MLINKS+=pwcache.3 user_from_uid.3 pwcache.3 group_from_gid.3

.include <bsd.lib.mk>
