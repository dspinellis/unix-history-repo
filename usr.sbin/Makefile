#	@(#)Makefile	5.6.1.2 (Berkeley) 5/8/91
#
# PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
# --------------------         -----   ----------------------
# CURRENT PATCH LEVEL:         3       00162
# --------------------         -----   ----------------------
#
# 09 Mar 93	Rodney W. Grimes	Added rwhod to SUBDIR, moved portmap
#					into alphabetic order.
# 03 Mar 93	Rodney W. Grimes	Added 11 new programs from Net/2
# 26 May 93	Rodney W. Grimes	Added keymap for codrv
#

SUBDIR=	accton arp chown chroot config dbsym dev_mkdb diskpart \
	edquota flcopy gettable htable inetd iostat kgmon kvm_mkdb lpr \
	mtree named portmap pwd_mkdb quotaon repquota rmt rwhod\
	sendmail sliplogin syslogd timed traceroute trpt trsp update vipw

.if   make(clean) || make(cleandir)
SUBDIR+=bad144
.elif ${MACHINE} == "i386"
SUBDIR+=bad144 
.endif

.include <bsd.subdir.mk>
