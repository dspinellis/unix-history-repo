#	@(#)Makefile	5.4 (Berkeley) %G%

NOOBJ=	oobj

# -rw-r--r--
BIN1=	aliases crontab daily ftpusers group hosts hosts.equiv hosts.lpd \
	inetd.conf ../usr.bin/man/man/man.conf monthly motd netstart \
	printcap protocols rc rc.local security services shells syslog.conf \
	ttys weekly

# -rw-rw-rw-
BIN2=	motd

# -rw-r--r--
ROOT1=	master.passwd

MTREE=	BSD.root.dist BSD.usr.dist BSD.var.dist

all clean cleandir depend etc install lint:

distribution:
	install -c -o ${BINOWN} -g ${BINGRP} -m 644 ${BIN1} ${DESTDIR}/etc
	install -c -o ${BINOWN} -g ${BINGRP} -m 666 ${BIN2} ${DESTDIR}/etc
	install -c -o root -g wheel -m 644 ${ROOT1} ${DESTDIR}/etc
	install -c -o ${BINOWN} -g ${BINGRP} -m 555 etc.${MACHINE}/MAKEDEV \
	    ${DESTDIR}/dev
	install -c -o ${BINOWN} -g ${BINGRP} -m 664 root/dot.cshrc \
	    ${DESTDIR}/root/.cshrc
	install -c -o ${BINOWN} -g ${BINGRP} -m 664 root/dot.login \
	    ${DESTDIR}/root/.login
	install -c -o ${BINOWN} -g ${BINGRP} -m 664 root/dot.profile \
	    ${DESTDIR}/root/.profile
	cd mtree; install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${MTREE} \
	    ${DESTDIR}/etc/mtree
	install -c -o ${BINOWN} -g operator -m 664 /dev/null \
	    ${DESTDIR}/etc/dumpdates
	install -c -o ${BINOWN} -g ${BINGRP} -m 664 /dev/null \
	    ${DESTDIR}/var/log/messages
	install -c -o ${BINOWN} -g ${BINGRP} -m 664 /dev/null \
	    ${DESTDIR}/var/log/maillog
	install -c -o ${BINOWN} -g ${BINGRP} -m 664 /dev/null \
	    ${DESTDIR}/var/log/lpd-errs
	install -c -o ${BINOWN} -g ${BINGRP} -m 664 /dev/null \
	    ${DESTDIR}/var/run/utmp

.include <bsd.prog.mk>
