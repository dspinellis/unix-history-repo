#	@(#)Makefile	5.6.1.2 (Berkeley) %G%

SUBDIR=	accton arp chown chroot config dev_mkdb diskpart edquota \
	gettable htable implog implogd inetd iostat kgmon kvm_mkdb lpr \
	mtree named pwd_mkdb quotaon repquota rmt rwhod \
	sendmail sliplogin syslogd traceroute trpt trsp update vipw

.if   make(clean) || make(cleandir)
SUBDIR+=bad144 dlmpcc flcopy rxformat timed
.elif ${MACHINE} == "hp300"
SUBDIR+=timed
.elif ${MACHINE} == "tahoe"
SUBDIR+=dlmpcc timed
.elif ${MACHINE} == "vax"
SUBDIR+=bad144 flcopy rxformat timed
.elif ${MACHINE} == "i386"
SUBDIR+=bad144 
.endif

.include <bsd.subdir.mk>
