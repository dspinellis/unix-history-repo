#	@(#)Makefile	5.4 (Berkeley) %G%

SUBDIR=	arp bad144 cat cp dd disklabel dump ed fsck ftp ifconfig \
	init ls mkdir mknod mount mv netstat newfs ping restore rlogin \
	rm route rsh sh slattach stty sync tar telnet umount

.if make(clean) || make(cleandir)
SUBDIR+=bin
.endif

install:
	for i in ${SUBDIR}; do \
		mv $$i/obj/$$i bin/$$i \
	done

.include <bsd.subdir.mk>

