#	@(#)Makefile	5.6.1.2 (Berkeley) 5/8/91
#
# PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
# --------------------         -----   ----------------------
# CURRENT PATCH LEVEL:         1       00087
# --------------------         -----   ----------------------
#
# 09 Mar 93	Rodney W. Grimes	Added rwhod to SUBDIR, moved portmap
#					into alphabetic order.
#

SUBDIR=	arp chown chroot config dbsym dev_mkdb diskpart \
	inetd kvm_mkdb lpr \
	mtree named portmap pwd_mkdb rmt rwhod\
	sendmail sliplogin syslogd timed traceroute update vipw

.if   make(clean) || make(cleandir)
SUBDIR+=bad144
.elif ${MACHINE} == "i386"
SUBDIR+=bad144 
.endif

.include <bsd.subdir.mk>
