#	@(#)Makefile	8.1 (Berkeley) %G%

# Missing: 03.kdebug 09.uucpimpl 12.uchanges 13.kchanges 17.security
#	   18.password 19.porttour 21.uucpnet
BINDIR=	/usr/share/doc/smm
FILES=	00.contents Makefile Title
SUBDIR=	02.config 04.quotas 05.fsck 06.lpd 07.sendmailop \
	08.timedop 10.newsop 11.named \
	14.fastfs 15.net 16.sendmail \
	20.termdesc 22.timed

.if	${MACHINE} == "tahoe"
SUBDIR+=01.setup
.elif	${MACHINE} == "vax"
SUBDIR+=01.setup
.endif

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}

.include <bsd.subdir.mk>
