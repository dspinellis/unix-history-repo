#	@(#)Makefile	5.2 (Berkeley) %G%

PROG=	amd
MAN8=	amd.0
SRCS=	afs_ops.c am_ops.c clock.c util.c efs_ops.c mapc.c info_file.c \
	info_hes.c info_ndbm.c info_passwd.c info_nis.c map.c srvr_afs.c \
	srvr_nfs.c mntfs.c misc_rpc.c mount_fs.c mount_xdr.c mtab.c \
	mtab_aix.c mtab_bsd.c mtab_file.c mtab_ultrix.c nfs_ops.c \
	nfs_prot_svc.c nfs_start.c nfs_subr.c nfs_prot_xdr.c opts.c \
	pfs_ops.c rpc_fwd.c sched.c sfs_ops.c amq_svc.c amq_subr.c \
	umount_fs.c host_ops.c ufs_ops.c ifs_ops.c amd.c get_args.c \
	rcs_info.c restart.c
OBJS+=	vers.${PROG}.o
LDADD+=	-lrpc
CFLAGS+=-I${.CURDIR}/../rpcx
CFLAGS+=-I${.CURDIR}/../config
CFLAGS+=-I${.CURDIR}/../include
CFLAGS+=-DARCH_REP=\"${MACHINE}\"
CFLAGS+=-DOS_REP=\"bsd44\"
CFLAGS+=-DOS_HDR=\"os-bsd44.h\"
CFLAGS+=${CONFIG}
CLEANFILES+=vers.${PROG}.c vers.${PROG}.o

vers.${PROG}.c: /tmp
	@d=${.CURDIR}/ sh ${.CURDIR}/../config/newvers.sh ${PROG}

.PATH: ${.CURDIR}/../rpcx ${.CURDIR}/../config
.include "Makefile.config"
.include "../../Makefile.inc"
.include <bsd.prog.mk>
