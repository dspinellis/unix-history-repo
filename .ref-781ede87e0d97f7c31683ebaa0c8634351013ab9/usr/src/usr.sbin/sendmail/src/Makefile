#	@(#)Makefile	5.19 (Berkeley) %G%

PROG=	sendmail
CFLAGS+=-I${.CURDIR} -DVMUNIX
SRCS=	alias.c arpadate.c clock.c collect.c conf.c convtime.c daemon.c \
	deliver.c domain.c envelope.c err.c headers.c macro.c main.c \
	parseaddr.c queue.c readcf.c recipient.c savemail.c srvrsmtp.c \
	stab.c stats.c sysexits.c trace.c usersmtp.c util.c version.c
DPADD=	${LIBDBM} ${LIBCOMPAT} ${LIBUTIL}
LDADD=	-ldbm -lcompat -lutil
MAN1=	newaliases.0
MAN5=	aliases.0
MAN8=	sendmail.0 
LINKS=	${DESTDIR}/usr/sbin/sendmail ${DESTDIR}/usr/bin/newaliases \
	${DESTDIR}/usr/sbin/sendmail ${DESTDIR}/usr/bin/mailq
BINDIR=	/usr/sbin
BINOWN=	root
BINGRP=	kmem
BINMODE=6555

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 644 /dev/null \
	    ${DESTDIR}/etc/sendmail.fc
	install -c -o ${BINOWN} -g ${BINGRP} -m 666 /dev/null \
	    ${DESTDIR}/var/log/sendmail.st
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/sendmail.hf \
	    ${DESTDIR}/usr/share/misc

.include <bsd.prog.mk>
