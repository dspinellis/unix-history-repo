#	@(#)Makefile	5.1 (Berkeley) 6/30/90

# 01.setup isn't right yet.
BINDIR=	/usr/share/doc/smm
FILES=	00.contents Makefile Title
SUBDIR=	02.config 03.kdebug 04.quotas 05.fsck 06.lpd 07.sendmailop \
	08.timedop 09.uucpimpl 10.newsop 11.named 12.uchanges 13.kchanges \
	14.fastfs 15.net 16.sendmail 17.security 18.password 19.porttour \
	20.termdesc 21.uucpnet 22.timed

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}

.include <bsd.subdir.mk>
