#
# Copyright (c) 1980, 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.26 (Berkeley) 5/5/88
#
DESTDIR=
CFLAGS=	-O
LIBC=	/lib/libc.a

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	config dump fsck ftpd getty htable implog named newfs \
	restore routed talkd termcap tftpd timed tzone XNSrouted \
	etc.${MACHINE}

# Shell scripts that need only be installed and are never removed.
#
SCRIPT=	fastboot fasthalt mklost+found

# C sources that live in the current directory
#
SRCS=	ac.c accton.c badsect.c chown.c clri.c comsat.c cron.c dcheck.c \
	disklabel.c diskpart.c dumpfs.c edquota.c fingerd.c fsdb.c fstat.c \
	gettable.c halt.c icheck.c ifconfig.c inetd.c mknod.c mkpasswd.c \
	mkproto.c mount.c ncheck.c quot.c quotacheck.c quotaon.c reboot.c \
	renice.c repquota.c rexecd.c rlogind.c rmt.c route.c rshd.c \
	rwhod.c sa.c savecore.c slattach.c swapon.c syslogd.c telnetd.c \
	tunefs.c umount.c update.c vipw.c arp.c dmesg.c init.c kgmon.c \
	ping.c pstat.c shutdown.c trpt.c

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	ac accton badsect chown clri comsat cron dcheck diskpart dumpfs \
	edquota fingerd fsdb gettable halt icheck ifconfig inetd mknod \
	mkpasswd mkproto mount ncheck quot quotacheck quotaon reboot renice \
	repquota rexecd rlogind rmt route rshd rwhod sa savecore slattach \
	swapon syslogd tunefs umount update vipw

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	telnetd

# Programs that get installed with special permissions
#
PERM=	arp dmesg disklabel fstat init kgmon ping pstat shutdown trpt

# optional: not always made
OPTIONAL= mkhosts

all:	${SUBDIR} ${STD} ${NSTD} ${PERM} ${OPTIONAL}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

telnetd:
	${CC} ${CFLAGS} -o $@ telnetd.c getty/get_date.c getty/gettytab.c

${STD} ${PERM}: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

clean: FRC
	-for i in ${SUBDIR} etc.vax etc.tahoe; do \
		(cd $$i; make ${MFLAGS} clean); \
	done
	rm -f a.out core *.s *.o ${STD} ${NSTD} ${PERM}

depend: ldepend
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done

ldepend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${SCRIPT}; do \
		(install -c -o bin -g bin -m 755 $$i.sh ${DESTDIR}/etc/$$i); \
	done
	install -s -o bin -g bin -m 755 ${STD} ${NSTD} ${DESTDIR}/etc
	-for i in ${OPTIONAL}; do \
		(if [ -f $$i ]; then \
			install -s -o bin -g bin -m 755 $$i ${DESTDIR}/etc; \
		fi); \
	done
# programs that run with special permissions
	install -s -o root -g bin -m 4755 -s ping ${DESTDIR}/etc/ping
	install -s -o root -g operator -m 4750 shutdown ${DESTDIR}/etc/shutdown
	install -s -o bin -g operator -m 2750 disklabel ${DESTDIR}/etc/disklabel
	install -s -o bin -g kmem -m 2755 pstat ${DESTDIR}/etc/pstat
	install -s -o bin -g kmem -m 2755 arp ${DESTDIR}/etc/arp
	install -s -o bin -g kmem -m 2755 dmesg ${DESTDIR}/etc/dmesg
	install -s -o bin -g kmem -m 2755 fstat ${DESTDIR}/etc/fstat
	install -s -o bin -g kmem -m 2755 kgmon ${DESTDIR}/etc/kgmon
	install -s -o bin -g kmem -m 2755 trpt ${DESTDIR}/etc/trpt
	install -s -o bin -g bin -m 700 init ${DESTDIR}/etc/init
# directories and links
	rm -f ${DESTDIR}/etc/quotaoff
	ln ${DESTDIR}/etc/quotaon ${DESTDIR}/etc/quotaoff
	rm -f ${DESTDIR}/bin/chgrp
	ln ${DESTDIR}/etc/chown ${DESTDIR}/bin/chgrp
	-[ -d ${DESTDIR}/usr/spool/rwho ] || mkdir ${DESTDIR}/usr/spool/rwho

FRC:

# Files listed in ${NSTD} and ${OPTIONAL} have explicit make lines given below.

mkhosts:
	@echo "Mkhosts only works with the dbm version of the hostent routines."
	@echo "If gethosts() is not in libc, mkhosts isn't installed."
	-${CC} ${CFLAGS} -o mkhosts mkhosts.c || rm mkhosts

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

ac: ac.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/utmp.h
ac: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/types.h
ac: /usr/include/sys/timeb.h
accton: accton.c
badsect: badsect.c /usr/include/stdio.h /usr/include/sys/param.h
badsect: /usr/include/sys/types.h /usr/include/signal.h
badsect: /usr/include/machine/trap.h /usr/include/machine/machparam.h
badsect: /usr/include/machine/endian.h /usr/include/sys/fs.h
badsect: /usr/include/sys/dir.h /usr/include/sys/stat.h
badsect: /usr/include/sys/inode.h
chown: chown.c /usr/include/sys/param.h /usr/include/sys/types.h
chown: /usr/include/signal.h /usr/include/machine/trap.h
chown: /usr/include/machine/machparam.h /usr/include/machine/endian.h
chown: /usr/include/sys/stat.h /usr/include/sys/dir.h /usr/include/pwd.h
chown: /usr/include/grp.h /usr/include/stdio.h /usr/include/ctype.h
clri: clri.c /usr/include/sys/param.h /usr/include/sys/types.h
clri: /usr/include/signal.h /usr/include/machine/trap.h
clri: /usr/include/machine/machparam.h /usr/include/machine/endian.h
clri: /usr/include/sys/inode.h /usr/include/sys/fs.h
comsat: comsat.c /usr/include/sys/param.h /usr/include/sys/types.h
comsat: /usr/include/signal.h /usr/include/machine/trap.h
comsat: /usr/include/machine/machparam.h /usr/include/machine/endian.h
comsat: /usr/include/sys/socket.h /usr/include/sys/stat.h
comsat: /usr/include/sys/file.h /usr/include/sys/wait.h
comsat: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/sgtty.h
comsat: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
comsat: /usr/include/sys/ttydev.h /usr/include/utmp.h /usr/include/signal.h
comsat: /usr/include/errno.h /usr/include/netdb.h /usr/include/syslog.h
comsat: /usr/include/strings.h
cron: cron.c /usr/include/sys/types.h /usr/include/stdio.h /usr/include/ctype.h
cron: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/sys/time.h
cron: /usr/include/time.h /usr/include/sys/stat.h /usr/include/sys/wait.h
cron: /usr/include/machine/endian.h /usr/include/sys/ioctl.h
cron: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cron: /usr/include/sys/file.h /usr/include/sys/resource.h /usr/include/pwd.h
cron: /usr/include/fcntl.h /usr/include/syslog.h
dcheck: dcheck.c /usr/include/sys/param.h /usr/include/sys/types.h
dcheck: /usr/include/signal.h /usr/include/machine/trap.h
dcheck: /usr/include/machine/machparam.h /usr/include/machine/endian.h
dcheck: /usr/include/sys/inode.h /usr/include/sys/fs.h /usr/include/sys/dir.h
dcheck: /usr/include/stdio.h
disklabel: disklabel.c /usr/include/stdio.h /usr/include/ctype.h
disklabel: /usr/include/sys/param.h /usr/include/sys/types.h
disklabel: /usr/include/signal.h /usr/include/machine/trap.h
disklabel: /usr/include/machine/machparam.h /usr/include/machine/endian.h
disklabel: /usr/include/sys/signal.h /usr/include/sys/errno.h
disklabel: /usr/include/sys/file.h /usr/include/sys/ioctl.h
disklabel: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
disklabel: /usr/include/sys/fs.h /usr/include/strings.h
disklabel: /usr/include/sys/disklabel.h
diskpart: diskpart.c /usr/include/sys/param.h /usr/include/sys/types.h
diskpart: /usr/include/signal.h /usr/include/machine/trap.h
diskpart: /usr/include/machine/machparam.h /usr/include/machine/endian.h
diskpart: /usr/include/sys/disklabel.h /usr/include/stdio.h
diskpart: /usr/include/ctype.h
dumpfs: dumpfs.c /usr/include/sys/param.h /usr/include/sys/types.h
dumpfs: /usr/include/signal.h /usr/include/machine/trap.h
dumpfs: /usr/include/machine/machparam.h /usr/include/machine/endian.h
dumpfs: /usr/include/sys/inode.h /usr/include/sys/fs.h /usr/include/stdio.h
dumpfs: /usr/include/fstab.h
edquota: edquota.c /usr/include/stdio.h /usr/include/signal.h
edquota: /usr/include/machine/trap.h /usr/include/errno.h /usr/include/pwd.h
edquota: /usr/include/ctype.h /usr/include/fstab.h /usr/include/sys/param.h
edquota: /usr/include/sys/types.h /usr/include/signal.h
edquota: /usr/include/machine/machparam.h /usr/include/machine/endian.h
edquota: /usr/include/sys/stat.h /usr/include/sys/file.h
edquota: /usr/include/sys/quota.h
fingerd: fingerd.c /usr/include/sys/types.h /usr/include/netinet/in.h
fingerd: /usr/include/stdio.h /usr/include/ctype.h
fsdb: fsdb.c /usr/include/sys/param.h /usr/include/sys/types.h
fsdb: /usr/include/signal.h /usr/include/machine/trap.h
fsdb: /usr/include/machine/machparam.h /usr/include/machine/endian.h
fsdb: /usr/include/signal.h /usr/include/sys/fs.h /usr/include/sys/inode.h
fsdb: /usr/include/sys/dir.h /usr/include/sys/file.h /usr/include/stdio.h
fsdb: /usr/include/setjmp.h
fstat: fstat.c /usr/include/machine/pte.h /usr/include/sys/param.h
fstat: /usr/include/sys/types.h /usr/include/signal.h
fstat: /usr/include/machine/trap.h /usr/include/machine/machparam.h
fstat: /usr/include/machine/endian.h /usr/include/sys/dir.h
fstat: /usr/include/sys/user.h /usr/include/machine/pcb.h
fstat: /usr/include/sys/dmap.h /usr/include/sys/time.h /usr/include/time.h
fstat: /usr/include/sys/resource.h /usr/include/sys/namei.h
fstat: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/sys/proc.h
fstat: /usr/include/sys/text.h /usr/include/sys/stat.h /usr/include/sys/inode.h
fstat: /usr/include/sys/socket.h /usr/include/sys/socketvar.h
fstat: /usr/include/sys/domain.h /usr/include/sys/protosw.h
fstat: /usr/include/sys/unpcb.h /usr/include/sys/vmmac.h
fstat: /usr/include/sys/file.h /usr/include/net/route.h
fstat: /usr/include/netinet/in.h /usr/include/netinet/in_pcb.h
fstat: /usr/include/stdio.h /usr/include/ctype.h /usr/include/nlist.h
fstat: /usr/include/pwd.h
gettable: gettable.c /usr/include/sys/types.h /usr/include/sys/socket.h
gettable: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/netdb.h
halt: halt.c /usr/include/stdio.h /usr/include/sys/reboot.h
halt: /usr/include/sys/types.h /usr/include/sys/time.h /usr/include/time.h
halt: /usr/include/sys/syslog.h /usr/include/errno.h /usr/include/signal.h
halt: /usr/include/machine/trap.h /usr/include/pwd.h /usr/include/utmp.h
icheck: icheck.c /usr/include/stdio.h /usr/include/sys/param.h
icheck: /usr/include/sys/types.h /usr/include/signal.h
icheck: /usr/include/machine/trap.h /usr/include/machine/machparam.h
icheck: /usr/include/machine/endian.h /usr/include/sys/inode.h
icheck: /usr/include/sys/fs.h
ifconfig: ifconfig.c /usr/include/sys/types.h /usr/include/sys/socket.h
ifconfig: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ifconfig: /usr/include/sys/ttydev.h /usr/include/net/if.h
ifconfig: /usr/include/net/if_arp.h /usr/include/netinet/in.h
ifconfig: /usr/include/netns/ns.h /usr/include/netns/ns_if.h
ifconfig: /usr/include/stdio.h /usr/include/errno.h /usr/include/ctype.h
ifconfig: /usr/include/netdb.h
inetd: inetd.c /usr/include/sys/param.h /usr/include/sys/types.h
inetd: /usr/include/signal.h /usr/include/machine/trap.h
inetd: /usr/include/machine/machparam.h /usr/include/machine/endian.h
inetd: /usr/include/sys/stat.h /usr/include/sys/ioctl.h
inetd: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
inetd: /usr/include/sys/socket.h /usr/include/sys/file.h
inetd: /usr/include/sys/wait.h /usr/include/sys/time.h /usr/include/time.h
inetd: /usr/include/sys/resource.h /usr/include/netinet/in.h
inetd: /usr/include/arpa/inet.h /usr/include/errno.h /usr/include/stdio.h
inetd: /usr/include/signal.h /usr/include/netdb.h /usr/include/syslog.h
inetd: /usr/include/pwd.h /usr/include/ctype.h
mknod: mknod.c /usr/include/stdio.h
mkpasswd: mkpasswd.c /usr/include/sys/file.h /usr/include/stdio.h
mkpasswd: /usr/include/pwd.h /usr/include/ndbm.h
mkproto: mkproto.c /usr/include/stdio.h /usr/include/sys/param.h
mkproto: /usr/include/sys/types.h /usr/include/signal.h
mkproto: /usr/include/machine/trap.h /usr/include/machine/machparam.h
mkproto: /usr/include/machine/endian.h /usr/include/sys/inode.h
mkproto: /usr/include/sys/fs.h /usr/include/sys/dir.h
mount: mount.c /usr/include/sys/param.h /usr/include/sys/types.h
mount: /usr/include/signal.h /usr/include/machine/trap.h
mount: /usr/include/machine/machparam.h /usr/include/machine/endian.h
mount: /usr/include/stdio.h /usr/include/fstab.h /usr/include/mtab.h
mount: /usr/include/errno.h
ncheck: ncheck.c /usr/include/sys/param.h /usr/include/sys/types.h
ncheck: /usr/include/signal.h /usr/include/machine/trap.h
ncheck: /usr/include/machine/machparam.h /usr/include/machine/endian.h
ncheck: /usr/include/sys/inode.h /usr/include/sys/fs.h /usr/include/sys/dir.h
ncheck: /usr/include/stdio.h
quot: quot.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/sys/param.h
quot: /usr/include/sys/types.h /usr/include/signal.h
quot: /usr/include/machine/trap.h /usr/include/machine/machparam.h
quot: /usr/include/machine/endian.h /usr/include/sys/inode.h
quot: /usr/include/sys/fs.h /usr/include/sys/file.h /usr/include/sys/dir.h
quot: /usr/include/fstab.h /usr/include/pwd.h /usr/include/grp.h
quot: /usr/include/utmp.h
quotacheck: quotacheck.c /usr/include/stdio.h /usr/include/ctype.h
quotacheck: /usr/include/signal.h /usr/include/machine/trap.h
quotacheck: /usr/include/errno.h /usr/include/sys/param.h
quotacheck: /usr/include/sys/types.h /usr/include/signal.h
quotacheck: /usr/include/machine/machparam.h /usr/include/machine/endian.h
quotacheck: /usr/include/sys/inode.h /usr/include/sys/fs.h
quotacheck: /usr/include/sys/quota.h /usr/include/sys/stat.h
quotacheck: /usr/include/sys/wait.h /usr/include/fstab.h /usr/include/pwd.h
quotaon: quotaon.c /usr/include/sys/param.h /usr/include/sys/types.h
quotaon: /usr/include/signal.h /usr/include/machine/trap.h
quotaon: /usr/include/machine/machparam.h /usr/include/machine/endian.h
quotaon: /usr/include/sys/file.h /usr/include/stdio.h /usr/include/fstab.h
quotaon: /usr/include/mtab.h
reboot: reboot.c /usr/include/stdio.h /usr/include/sys/reboot.h
reboot: /usr/include/errno.h /usr/include/signal.h /usr/include/machine/trap.h
reboot: /usr/include/pwd.h /usr/include/sys/types.h /usr/include/sys/time.h
reboot: /usr/include/time.h /usr/include/sys/syslog.h /usr/include/utmp.h
renice: renice.c /usr/include/sys/time.h /usr/include/time.h
renice: /usr/include/sys/resource.h /usr/include/stdio.h /usr/include/pwd.h
repquota: repquota.c /usr/include/stdio.h /usr/include/errno.h
repquota: /usr/include/sys/param.h /usr/include/sys/types.h
repquota: /usr/include/signal.h /usr/include/machine/trap.h
repquota: /usr/include/machine/machparam.h /usr/include/machine/endian.h
repquota: /usr/include/sys/quota.h /usr/include/sys/stat.h /usr/include/fstab.h
repquota: /usr/include/pwd.h
rexecd: rexecd.c /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
rexecd: /usr/include/sys/ttydev.h /usr/include/sys/param.h
rexecd: /usr/include/sys/types.h /usr/include/signal.h
rexecd: /usr/include/machine/trap.h /usr/include/machine/machparam.h
rexecd: /usr/include/machine/endian.h /usr/include/sys/socket.h
rexecd: /usr/include/sys/time.h /usr/include/time.h /usr/include/netinet/in.h
rexecd: /usr/include/stdio.h /usr/include/errno.h /usr/include/pwd.h
rexecd: /usr/include/signal.h /usr/include/netdb.h
rlogind: rlogind.c /usr/include/stdio.h /usr/include/sys/types.h
rlogind: /usr/include/sys/stat.h /usr/include/sys/socket.h
rlogind: /usr/include/sys/wait.h /usr/include/machine/endian.h
rlogind: /usr/include/sys/file.h /usr/include/netinet/in.h /usr/include/errno.h
rlogind: /usr/include/pwd.h /usr/include/signal.h /usr/include/machine/trap.h
rlogind: /usr/include/sgtty.h /usr/include/sys/ioctl.h
rlogind: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
rlogind: /usr/include/stdio.h /usr/include/netdb.h /usr/include/syslog.h
rlogind: /usr/include/strings.h /usr/include/utmp.h
rmt: rmt.c /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
rmt: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
rmt: /usr/include/sys/types.h /usr/include/sys/socket.h /usr/include/sys/mtio.h
rmt: /usr/include/errno.h
route: route.c /usr/include/sys/param.h /usr/include/sys/types.h
route: /usr/include/signal.h /usr/include/machine/trap.h
route: /usr/include/machine/machparam.h /usr/include/machine/endian.h
route: /usr/include/sys/socket.h /usr/include/sys/ioctl.h
route: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
route: /usr/include/sys/mbuf.h /usr/include/net/route.h
route: /usr/include/netinet/in.h /usr/include/netns/ns.h /usr/include/stdio.h
route: /usr/include/errno.h /usr/include/ctype.h /usr/include/netdb.h
route: /usr/include/nlist.h
rshd: rshd.c /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
rshd: /usr/include/sys/ttydev.h /usr/include/sys/param.h
rshd: /usr/include/sys/types.h /usr/include/signal.h
rshd: /usr/include/machine/trap.h /usr/include/machine/machparam.h
rshd: /usr/include/machine/endian.h /usr/include/sys/socket.h
rshd: /usr/include/sys/file.h /usr/include/sys/time.h /usr/include/time.h
rshd: /usr/include/netinet/in.h /usr/include/arpa/inet.h /usr/include/stdio.h
rshd: /usr/include/errno.h /usr/include/pwd.h /usr/include/signal.h
rshd: /usr/include/netdb.h /usr/include/syslog.h
rwhod: rwhod.c /usr/include/sys/types.h /usr/include/sys/socket.h
rwhod: /usr/include/sys/stat.h /usr/include/sys/ioctl.h
rwhod: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
rwhod: /usr/include/sys/file.h /usr/include/net/if.h /usr/include/net/if_arp.h
rwhod: /usr/include/netinet/in.h /usr/include/nlist.h /usr/include/stdio.h
rwhod: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/errno.h
rwhod: /usr/include/utmp.h /usr/include/ctype.h /usr/include/netdb.h
rwhod: /usr/include/syslog.h /usr/include/protocols/rwhod.h
sa: sa.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/sys/types.h
sa: /usr/include/sys/acct.h /usr/include/signal.h /usr/include/machine/trap.h
sa: /usr/include/utmp.h /usr/include/pwd.h
savecore: savecore.c /usr/include/stdio.h /usr/include/nlist.h
savecore: /usr/include/sys/param.h /usr/include/sys/types.h
savecore: /usr/include/signal.h /usr/include/machine/trap.h
savecore: /usr/include/machine/machparam.h /usr/include/machine/endian.h
savecore: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/sys/fs.h
savecore: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/file.h
savecore: /usr/include/sys/syslog.h
slattach: slattach.c /usr/include/stdio.h /usr/include/sys/param.h
slattach: /usr/include/sys/types.h /usr/include/signal.h
slattach: /usr/include/machine/trap.h /usr/include/machine/machparam.h
slattach: /usr/include/machine/endian.h /usr/include/sgtty.h
slattach: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
slattach: /usr/include/sys/ttydev.h /usr/include/sys/socket.h
slattach: /usr/include/netinet/in.h /usr/include/net/if.h
slattach: /usr/include/net/if_arp.h /usr/include/netdb.h /usr/include/fcntl.h
swapon: swapon.c /usr/include/fstab.h /usr/include/errno.h /usr/include/stdio.h
syslogd: syslogd.c /usr/include/stdio.h /usr/include/utmp.h
syslogd: /usr/include/ctype.h /usr/include/strings.h /usr/include/setjmp.h
syslogd: /usr/include/sys/syslog.h /usr/include/sys/param.h
syslogd: /usr/include/sys/types.h /usr/include/signal.h
syslogd: /usr/include/machine/trap.h /usr/include/machine/machparam.h
syslogd: /usr/include/machine/endian.h /usr/include/sys/errno.h
syslogd: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
syslogd: /usr/include/sys/ttydev.h /usr/include/sys/stat.h
syslogd: /usr/include/sys/wait.h /usr/include/sys/socket.h
syslogd: /usr/include/sys/file.h /usr/include/sys/msgbuf.h
syslogd: /usr/include/sys/uio.h /usr/include/sys/un.h /usr/include/sys/time.h
syslogd: /usr/include/time.h /usr/include/sys/resource.h
syslogd: /usr/include/sys/signal.h /usr/include/netinet/in.h
syslogd: /usr/include/netdb.h
telnetd: telnetd.c /usr/include/sys/param.h /usr/include/sys/types.h
telnetd: /usr/include/signal.h /usr/include/machine/trap.h
telnetd: /usr/include/machine/machparam.h /usr/include/machine/endian.h
telnetd: /usr/include/sys/socket.h /usr/include/sys/wait.h
telnetd: /usr/include/sys/file.h /usr/include/sys/stat.h
telnetd: /usr/include/sys/time.h /usr/include/time.h /usr/include/netinet/in.h
telnetd: /usr/include/arpa/telnet.h /usr/include/stdio.h /usr/include/signal.h
telnetd: /usr/include/errno.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
telnetd: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
telnetd: /usr/include/netdb.h /usr/include/syslog.h /usr/include/ctype.h
telnetd: /usr/include/utmp.h
tunefs: tunefs.c /usr/include/sys/param.h /usr/include/sys/types.h
tunefs: /usr/include/signal.h /usr/include/machine/trap.h
tunefs: /usr/include/machine/machparam.h /usr/include/machine/endian.h
tunefs: /usr/include/sys/stat.h /usr/include/sys/fs.h /usr/include/sys/inode.h
tunefs: /usr/include/stdio.h /usr/include/fstab.h
umount: umount.c /usr/include/sys/param.h /usr/include/sys/types.h
umount: /usr/include/signal.h /usr/include/machine/trap.h
umount: /usr/include/machine/machparam.h /usr/include/machine/endian.h
umount: /usr/include/stdio.h /usr/include/fstab.h /usr/include/mtab.h
update: update.c /usr/include/sys/time.h /usr/include/time.h
update: /usr/include/sys/file.h /usr/include/sys/signal.h
update: /usr/include/machine/trap.h /usr/include/syslog.h /usr/include/stdio.h
vipw: vipw.c /usr/include/machine/machparam.h /usr/include/machine/endian.h
vipw: /usr/include/sys/types.h /usr/include/sys/stat.h
vipw: /usr/include/sys/signal.h /usr/include/machine/trap.h
vipw: /usr/include/sys/file.h /usr/include/stdio.h /usr/include/errno.h
arp: arp.c /usr/include/machine/pte.h /usr/include/sys/param.h
arp: /usr/include/sys/types.h /usr/include/signal.h /usr/include/machine/trap.h
arp: /usr/include/machine/machparam.h /usr/include/machine/endian.h
arp: /usr/include/sys/vmmac.h /usr/include/sys/file.h /usr/include/sys/socket.h
arp: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
arp: /usr/include/sys/ttydev.h /usr/include/netdb.h /usr/include/netinet/in.h
arp: /usr/include/net/if.h /usr/include/net/if_arp.h
arp: /usr/include/netinet/if_ether.h /usr/include/errno.h /usr/include/nlist.h
arp: /usr/include/stdio.h
dmesg: dmesg.c /usr/include/stdio.h /usr/include/sys/param.h
dmesg: /usr/include/sys/types.h /usr/include/signal.h
dmesg: /usr/include/machine/trap.h /usr/include/machine/machparam.h
dmesg: /usr/include/machine/endian.h /usr/include/nlist.h /usr/include/signal.h
dmesg: /usr/include/sys/file.h /usr/include/sys/vm.h /usr/include/sys/vmparam.h
dmesg: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
dmesg: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h
dmesg: /usr/include/sys/msgbuf.h
init: init.c /usr/include/signal.h /usr/include/machine/trap.h
init: /usr/include/sys/types.h /usr/include/utmp.h /usr/include/setjmp.h
init: /usr/include/sys/reboot.h /usr/include/errno.h /usr/include/sys/file.h
init: /usr/include/ttyent.h /usr/include/sys/syslog.h /usr/include/sys/stat.h
kgmon: kgmon.c /usr/include/sys/param.h /usr/include/sys/types.h
kgmon: /usr/include/signal.h /usr/include/machine/trap.h
kgmon: /usr/include/machine/machparam.h /usr/include/machine/endian.h
kgmon: /usr/include/machine/pte.h /usr/include/sys/file.h /usr/include/sys/vm.h
kgmon: /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
kgmon: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
kgmon: /usr/include/sys/vmsystm.h /usr/include/stdio.h /usr/include/nlist.h
kgmon: /usr/include/ctype.h /usr/include/sys/gprof.h
ping: ping.c /usr/include/stdio.h /usr/include/errno.h /usr/include/sys/time.h
ping: /usr/include/time.h /usr/include/sys/param.h /usr/include/sys/types.h
ping: /usr/include/signal.h /usr/include/machine/trap.h
ping: /usr/include/machine/machparam.h /usr/include/machine/endian.h
ping: /usr/include/sys/socket.h /usr/include/sys/file.h
ping: /usr/include/netinet/in_systm.h /usr/include/netinet/in.h
ping: /usr/include/netinet/ip.h /usr/include/netinet/ip_icmp.h
ping: /usr/include/netdb.h
pstat: pstat.c /usr/include/sys/param.h /usr/include/sys/types.h
pstat: /usr/include/signal.h /usr/include/machine/trap.h
pstat: /usr/include/machine/machparam.h /usr/include/machine/endian.h
pstat: /usr/include/sys/dir.h /usr/include/sys/file.h /usr/include/sys/user.h
pstat: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
pstat: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/resource.h
pstat: /usr/include/sys/namei.h /usr/include/sys/uio.h /usr/include/errno.h
pstat: /usr/include/sys/proc.h /usr/include/sys/text.h /usr/include/sys/inode.h
pstat: /usr/include/sys/map.h /usr/include/sys/ioctl.h
pstat: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
pstat: /usr/include/sys/tty.h /usr/include/sys/ttychars.h
pstat: /usr/include/sys/ttydev.h /usr/include/sys/conf.h /usr/include/sys/vm.h
pstat: /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
pstat: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
pstat: /usr/include/sys/vmsystm.h /usr/include/nlist.h
pstat: /usr/include/machine/pte.h /usr/include/stdio.h
shutdown: shutdown.c /usr/include/stdio.h /usr/include/ctype.h
shutdown: /usr/include/signal.h /usr/include/machine/trap.h
shutdown: /usr/include/setjmp.h /usr/include/utmp.h /usr/include/pwd.h
shutdown: /usr/include/sys/time.h /usr/include/time.h
shutdown: /usr/include/sys/resource.h /usr/include/sys/param.h
shutdown: /usr/include/sys/types.h /usr/include/signal.h
shutdown: /usr/include/machine/machparam.h /usr/include/machine/endian.h
shutdown: /usr/include/sys/syslog.h
trpt: trpt.c /usr/include/machine/pte.h /usr/include/sys/param.h
trpt: /usr/include/sys/types.h /usr/include/signal.h
trpt: /usr/include/machine/trap.h /usr/include/machine/machparam.h
trpt: /usr/include/machine/endian.h /usr/include/sys/vmmac.h
trpt: /usr/include/sys/socket.h /usr/include/sys/socketvar.h
trpt: /usr/include/sys/protosw.h /usr/include/sys/file.h
trpt: /usr/include/net/route.h /usr/include/net/if.h /usr/include/net/if_arp.h
trpt: /usr/include/netinet/in.h /usr/include/netinet/in_pcb.h
trpt: /usr/include/netinet/in_systm.h /usr/include/netinet/ip.h
trpt: /usr/include/netinet/ip_var.h /usr/include/netinet/tcp.h
trpt: /usr/include/netinet/tcp_fsm.h /usr/include/netinet/tcp_seq.h
trpt: /usr/include/netinet/tcp_timer.h /usr/include/netinet/tcp_var.h
trpt: /usr/include/netinet/tcpip.h /usr/include/netinet/tcp_debug.h
trpt: /usr/include/arpa/inet.h /usr/include/stdio.h /usr/include/errno.h
trpt: /usr/include/nlist.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
