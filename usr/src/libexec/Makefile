#	@(#)Makefile	5.9 (Berkeley) %G%

SUBDIR=	bugfiler comsat fingerd ftpd getNAME getty kpasswdd lfs_cleanerd \
	mail.local makekey rexecd rlogind rshd talkd telnetd tftpd uucpd

#KERBEROS=kpasswdd 

.if	${MACHINE} == "hp300"
SUBDIR+=rbootd
.endif

.include <bsd.subdir.mk>
