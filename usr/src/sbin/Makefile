#	@(#)Makefile	5.4.1.1 (Berkeley) 5/7/91

# Missing: icheck, init, ncheck

SUBDIR=	XNSrouted badsect clri disklabel dmesg dump dumpfs fastboot fsck \
	halt ifconfig mknod mount mountd newfs \
	nfsd nfsiod ping quotacheck reboot restore route routed savecore \
	shutdown slattach swapon tunefs umount

.if   make(clean) || make(cleandir)
SUBDIR+=enpload
.elif ${MACHINE} == "tahoe"
SUBDIR+=enpload
.endif

.include <bsd.subdir.mk>
