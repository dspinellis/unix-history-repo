#	@(#)Makefile	5.6.1.2 (Berkeley) 5/8/91

SUBDIR=	XNSrouted ac accton arp chown chroot config cron dbsym dev_mkdb \
	diskpart edquota fdcontrol fdformat flcopy ftinfo gettable htable \
	inetd iostat kbdcontrol kgmon kvm_mkdb lpr lptcontrol mrouted \
	mtree named portmap pppstats pwd_mkdb quotaon repquota rmt \
	routed rwhod sa sendmail sliplogin spkrtest swapinfo syslogd \
	timed traceroute trpt trsp vidcontrol vipw yp

.if   make(clean) || make(cleandir)
SUBDIR+=bad144
.elif ${MACHINE} == "i386"
SUBDIR+=bad144 
.endif

.include <bsd.subdir.mk>
