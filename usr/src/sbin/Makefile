#	@(#)Makefile	5.2 (Berkeley) %G%

SUBDIR=	XNSrouted badsect clri disklabel dmesg dump dumpfs fastboot fsck \
	fsdb halt icheck ifconfig init mknod mount mountd ncheck newfs \
	nfsd nfsiod ping quotacheck reboot restore route routed savecore \
	shutdown slattach swapon tunefs umount vipw

.if ${MACHINE} == "tahoe" || make(clean) || make(cleandir)
SUBDIR+=enpload
.endif

.include <bsd.subdir.mk>
