#	@(#)Makefile	5.1 (Berkeley) %G%

# not ours?: gated
# not converted: gated
SUBDIR=	ac accton arp arp_iso chown chroot config cron diskpart edquota \
	gettable htable implog implogd inetd iostat kgmon kvm_mkdb lpr \
	mkhosts mkpasswd mkproto mtree quot quotaon rmt rwhod sa sendmail \
	sliplogin syslogd timed timedc traceroute trpt trsp update

.if ${MACHINE} == "tahoe" || make(clean) || make(cleandir)
SUBDIR+=dlmpcc
.endif

.if ${MACHINE} == "vax" || make(clean) || make(cleandir)
SUBDIR+=arff bad144 flcopy rxformat
.endif

.include <bsd.subdir.mk>
