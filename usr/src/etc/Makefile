#	@(#)Makefile	8.3 (Berkeley) %G%

NOOBJ=	oobj

# disktab may be wrong -- hcx9 is a tahoe, but gets its own.
# -rw-r--r--
BIN1=	aliases amd-home amd-n changelist csh.cshrc csh.login csh.logout \
	crontab crontab.local daily dm.conf exports ftpusers ftpwelcome \
	gettytab group hosts hosts.equiv hosts.lpd inetd.conf man.conf \
	monthly motd netgroup netstart phones printcap protocols rc \
	rc.local remote security services shells syslog.conf ttys weekly \
	etc.${MACHINE}/disktab

# -rw-rw-rw-
BIN2=	motd

MTREE=	4.4BSD.dist special
KRB=	README krb.conf krb.realms
NAMEDB=	localhost.rev named.boot root.cache
PCS=	pcs750.bin
WCS1=	wcs fppwcs poc poc1 poc2 fppoc
WCS2=	fpevent fppwcs fppwcs_dual hdcwcs load_diags start_fpp wcs wcs_dual
RBOOTD=	rbootd.conf

all clean cleandir depend etc install lint:

distribution:
	# Hand-craft the password file first.
	-mkdir /etc /dev
	cp master.passwd ${DESTDIR}/tmp/p
	pwd_mkdb -p /tmp/p
	rm -f /tmp/p
	cp group /etc/group
	# Build the tree.
	-mtree -def mtree/4.4BSD.dist -p ${DESTDIR}/ -u
	# install devices
.if ${MACHINE} == "hp300"
	(cd etc.hp300; install -c -o root -g wheel -m 444 \
	    MAKEDEV.hpux ${DESTDIR}/dev)
.endif
	install -c -o root -g wheel -m 555 \
	     MAKEDEV.local etc.${MACHINE}/MAKEDEV ${DESTDIR}/dev
	(cd /dev && ./MAKEDEV std)
	install -c -o root -g wheel -m 444 COPYRIGHT ${DESTDIR}/
	install -c -o root -g wheel -m 600 master.passwd ${DESTDIR}/etc
	install -c -o root -g wheel -m 644 ${BIN1} ${DESTDIR}/etc
	install -c -o root -g wheel -m 666 ${BIN2} ${DESTDIR}/etc
	(cd root; \
		install -c -o root -g wheel -m 644 dot.cshrc \
		    ${DESTDIR}/root/.cshrc; \
		install -c -o root -g wheel -m 600 dot.klogin \
		    ${DESTDIR}/root/.klogin; \
		install -c -o root -g wheel -m 644 dot.login \
		    ${DESTDIR}/root/.login; \
		install -c -o root -g wheel -m 644 dot.profile \
		    ${DESTDIR}/root/.profile; \
		install -c -o root -g wheel -m 600 dot.rhosts \
		    ${DESTDIR}/root/.rhosts; \
		rm -f ${DESTDIR}/.cshrc ${DESTDIR}/.profile; \
		ln ${DESTDIR}/root/.cshrc ${DESTDIR}/.cshrc; \
		ln ${DESTDIR}/root/.profile ${DESTDIR}/.profile)
	(cd mtree; install -c -o root -g wheel -m 444 ${MTREE} \
	    ${DESTDIR}/etc/mtree)
	(cd namedb; install -c -o root -g wheel -m 644 ${NAMEDB} \
	    ${DESTDIR}/etc/namedb)
	(cd kerberosIV; install -c -o root -g wheel -m 444 ${KRB} \
	    ${DESTDIR}/etc/kerberosIV)
	install -c -o root -g operator -m 664 /dev/null \
	    ${DESTDIR}/etc/dumpdates
	install -c -o root -g staff -m 644 /dev/null \
	    ${DESTDIR}/var/log/lastlog
	install -c -o root -g wheel -m 644 /dev/null \
	    ${DESTDIR}/var/log/messages
	install -c -o root -g wheel -m 644 /dev/null \
	    ${DESTDIR}/var/log/maillog
	install -c -o root -g wheel -m 644 /dev/null \
	    ${DESTDIR}/var/log/lpd-errs
	install -c -o root -g utmp -m 664 /dev/null \
	    ${DESTDIR}/var/run/utmp
	(cd etc.${MACHINE}; install -c -o root -g wheel -m 444 \
	    fstab.* ${DESTDIR}/etc)
.if ${MACHINE} == "hp300"
	(cd etc.hp300; install -c -o root -g wheel -m 444 ${RBOOTD} \
	    ${DESTDIR}/etc)
.endif
.if ${MACHINE} == "tahoe"
	(cd etc.tahoe; install -c -o root -g wheel -m 444 ${WCS1} \
	    ${DESTDIR}/)
.endif
.if ${MACHINE} == "vax"
	(cd etc.vax; install -c -o root -g wheel -m 444 ${PCS} \
	    ${DESTDIR}/)
.endif
	# There are certain symbolic links we have to have, and
	# no other place to have install them.
	(cd / && rm -f sys && ln -s usr/src/sys sys)
	(cd /etc && rm -f rmt && ln -s /usr/sbin/rmt rmt)
	(cd /etc && rm -f termcap && ln -s /usr/share/misc/termcap termcap)
	(cd /usr && rm -f X11 && ln -s X11R5 X11)
	(cd /usr/bin && rm -f X11 && ln -s /usr/X11/bin X11)
	(cd /usr/lib && rm -f X && ln -s /usr/hpux/lib/X X)
	(cd /usr/lib && rm -f X11 && ln -s /usr/X11/lib X11)
	(cd /usr/lib && rm -f grmd && ln -s /usr/hpux/lib/grmd grmd)
	(cd /usr/lib && rm -f tmac && ln -s /usr/share/tmac tmac)
	(cd /usr/ucb && rm -f rdist && ln -s /usr/bin/rdist rdist)
.if ${MACHINE} == "hp300"
	(cd /usr/bin && rm -f X11 && ln -s /usr/X11/bin X11)
.endif

hcx9-distribution:
	(cd etc.tahoe; install -c -o root -g wheel -m 444 ${WCS2} \
	    ${DESTDIR}/)

.include <bsd.prog.mk>
