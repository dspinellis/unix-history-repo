#	@(#)Makefile	5.6 (Berkeley) %G%

# Missing: icheck, init, ncheck

SUBDIR=	XNSrouted badsect clri disklabel dmesg dump dumpfs fastboot fsck fsdb \
	icheck ifconfig init mknod mount mount_fdesc mount_lofs mount_nfs \
	mountd ncheck newfs nfsd nfsiod nologin ping quotacheck \
	reboot restore route routed savecore shutdown slattach startslip \
	swapon tunefs umount

.if	${MACHINE} == "tahoe"
SUBDIR+=enpload
.elif	make(clean) || make(cleandir)
SUBDIR+=enpload
.endif

.include <bsd.subdir.mk>
