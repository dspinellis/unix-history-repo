#	@(#)Makefile	5.11 (Berkeley) %G%

LIB=	util
CFLAGS+=-DLIBC_SCCS -I/sys
SRCS=	daemon.c getloadavg.c login.c login_tty.c logout.c logwtmp.c \
	kvm.c kvm_${MACHINE}.c kvm_proc.c pty.c pwcache.c
MAN3=	getloadavg.0 kvm.0 kvm_geterr.0 kvm_getprocs.0 kvm_nlist.0 \
	kvm_open.0 pwcache.0

MLINKS+=pwcache.3 user_from_uid.3 pwcache.3 group_from_gid.3 \
	kvm.3 kvm_read.3 kvm.3 kvm_write.3 \
	kvm_open.3 kvm_openfiles.3 kvm_open.3 kvm_close.3 \
	kvm_getprocs.3 kvm_getargv.3 kvm_getprocs.3 kvm_getenvv.3

.include <bsd.lib.mk>
