#	@(#)Makefile	5.5 (Berkeley) %G%

SUBDIR=	ac accton arp chown chroot config cron diskpart edquota gettable \
	htable implog implogd inetd kgmon kvm_mkdb lpr mkhosts mkpasswd \
	mkproto mtree named pstat quot quotaon repquota rmt rwhod sa \
	sendmail sliplogin syslogd timed traceroute trpt trsp update

.if   make(clean) || make(cleandir)
SUBDIR+=arff bad144 dlmpcc flcopy iostat rxformat
.elif ${MACHINE} == "tahoe"
SUBDIR+=dlmpcc iostat
.elif ${MACHINE} == "vax"
SUBDIR+=arff bad144 flcopy iostat rxformat
.elif ${MACHINE} == "i386"
SUBDIR+=bad144 
.endif

.include <bsd.subdir.mk>
