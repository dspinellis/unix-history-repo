#	@(#)Makefile	5.5 (Berkeley) %G%

NOOBJ=	oobj

# -rw-r--r--
BIN1=	aliases csh.cshrc csh.login csh.logout crontab daily dm.conf \
	ftpusers gettytab group hosts hosts.equiv hosts.lpd inetd.conf \
	man.conf monthly motd netstart phones printcap protocols rc \
	rc.local remote security services shells syslog.conf ttys weekly

# -rw-rw-rw-
BIN2=	motd

DOT=	dot.cshrc dot.login dot.profile
MTREE=	BSD.root.dist BSD.usr.dist BSD.var.dist
KRB=	README krb.conf krb.realms

all clean cleandir depend etc install lint:

distribution:
	install -c -o ${BINOWN} -g ${BINGRP} -m 644 ${BIN1} ${DESTDIR}/etc
	install -c -o ${BINOWN} -g ${BINGRP} -m 666 ${BIN2} ${DESTDIR}/etc
	install -c -o root -g wheel -m 600 master.passwd ${DESTDIR}/etc
	(cd ${DESTDIR}/etc; \
	    mkpasswd -p master.passwd; \
	    mv master.passwd.pag passwd.pag; \
	    mv master.passwd.dir passwd.dir; \
	    mv master.passwd.orig passwd)
	install -c -o ${BINOWN} -g ${BINGRP} -m 555 etc.${MACHINE}/MAKEDEV \
	    ${DESTDIR}/dev
	cd root; install -c -o root -g wheel -m 644 ${DOT} ${DESTDIR}/root
	cd mtree; install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${MTREE} \
	    ${DESTDIR}/etc/mtree
	cd kerberosIV; install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${KRB} \
	    ${DESTDIR}/etc/kerberosIV
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
