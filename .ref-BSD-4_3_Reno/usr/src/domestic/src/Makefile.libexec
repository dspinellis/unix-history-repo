#	@(#)Makefile	5.6 (Berkeley) 7/2/90

SUBDIR=	bugfiler comsat delivermail fingerd ftpd getNAME getty makekey \
	rexecd rlogind rshd talkd telnetd tftpd uucpd kpasswdd

.if   ${MACHINE} == "hp300"
SUBDIR+=gcc
.elif ${MACHINE} == "tahoe"
SUBDIR+=pcc
.elif ${MACHINE} == "vax"
SUBDIR+=pcc
.endif

.include <bsd.subdir.mk>
