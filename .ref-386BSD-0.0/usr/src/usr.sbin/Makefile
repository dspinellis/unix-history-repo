#	@(#)Makefile	5.6.1.2 (Berkeley) 5/8/91

SUBDIR=	chown chroot config dev_mkdb diskpart \
	inetd kvm_mkdb lpr \
	portmap mtree named pwd_mkdb rmt \
	sendmail sliplogin syslogd timed traceroute update vipw

.if   make(clean) || make(cleandir)
SUBDIR+=bad144
.elif ${MACHINE} == "i386"
SUBDIR+=bad144 
.endif

.include <bsd.subdir.mk>
