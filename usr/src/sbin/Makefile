#	@(#)Makefile	5.4.1.1 (Berkeley) %G%

# Missing: icheck, init, ncheck

SUBDIR=	XNSrouted badsect clri disklabel dmesg dump dumpfs fastboot fsck \
	fsdb halt ifconfig mknod mount mountd newfs \
	nfsd nfsiod ping quotacheck reboot restore route routed savecore \
	shutdown slattach swapon tunefs umount

.if   make(clean) || make(cleandir)
SUBDIR+=enpload
.elif ${MACHINE} == "tahoe"
SUBDIR+=enpload
.endif

.include <bsd.subdir.mk>
