#	@(#)Makefile	5.6.1.1 (Berkeley) %G%

SUBDIR=	bugfiler comsat delivermail fingerd ftpd getNAME getty makekey \
	rexecd rlogind rshd talkd telnetd tftpd uucpd

#KERBEROS=kpasswdd

.if   ${MACHINE} == "hp300"
SUBDIR+=gcc
.elif ${MACHINE} == "tahoe"
SUBDIR+=pcc
.elif ${MACHINE} == "vax"
SUBDIR+=pcc
.endif

.include <bsd.subdir.mk>
