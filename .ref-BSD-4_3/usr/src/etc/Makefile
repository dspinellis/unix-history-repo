#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.14 (Berkeley) 5/16/86
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	config dump dump.4.1 fsck ftpd getty htable implog named restore \
	routed talkd termcap tftpd timed XNSrouted

# Shell scripts that need only be installed and are never removed.
#
SCRIPT=	fastboot fasthalt mklost+found

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	ac accton arff bad144 badsect catman chown clri comsat \
	cron dcheck diskpart dumpfs edquota fingerd flcopy \
	gettable ifconfig halt icheck inetd init newfs mkfs mknod \
	mkproto mount ncheck quot quotaon quotacheck reboot renice \
	repquota rexecd rlogind rmt route rshd rwhod rxformat sa savecore \
	shutdown swapon syslogd telnetd tunefs umount update vipw \
	mkpasswd

# C programs that live in the current directory and need explicit make lines.
#
NSTD=

# Programs that must run setuid to root
#
SETUID=	ping

# C programs that must run set-group-id kmem.
#
KMEM=	pstat arp dmesg kgmon trpt

# optional: not always made
OPTIONAL= mkhosts

all:	${SUBDIR} ${STD} ${NSTD} ${OPTIONAL} ${KMEM} ${SETUID}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

${STD} ${KMEM} ${SETUID}:
	cc ${CFLAGS} -o $@ $@.c

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${SCRIPT}; do (install -c $$i.sh ${DESTDIR}/etc/$$i); done
	-for i in ${STD} ${NSTD}; do (install -s $$i ${DESTDIR}/etc/$$i); done
	-for i in ${OPTIONAL}; do \
		(if [ -f $$i ]; then install -s $$i ${DESTDIR}/etc; fi); done
	-for i in ${SETUID}; do \
		(install -o root -m 4755 -s $$i ${DESTDIR}/etc/$$i); done
	-for i in ${KMEM}; do \
		(install -g kmem -m 2755 -s $$i ${DESTDIR}/etc/$$i); done
	-chown root.operator ${DESTDIR}/etc/shutdown
	-chmod 4750 ${DESTDIR}/etc/shutdown
	-rm -f ${DESTDIR}/etc/quotaoff
	-ln ${DESTDIR}/etc/quotaon ${DESTDIR}/etc/quotaoff
	-mkdir ${DESTDIR}/usr/spool/rwho
	-chmod 700 ${DESTDIR}/etc/init

clean:
	-rm -f a.out core *.s *.o
	-for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done
	-rm -f ${STD} ${NSTD} ${KMEM} ${SETUID}

FRC:

depend:
	for i in ${STD} ${NSTD} ${KMEM} ${SETUID}; do \
	    cc -M ${INCPATH} $$i.c | sed 's/\.o//' | \
	    awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# Files listed in ${NSTD} and ${OPTIONAL} have explicit make lines given below.

mkhosts:
	@echo "Mkhosts only works with the dbm version of the hostent routines."
	@echo "If gethosts() is not in libc, mkhosts isn't installed."
	-cc ${CFLAGS} -o mkhosts mkhosts.c || rm mkhosts

# DO NOT DELETE THIS LINE -- make depend uses it


ac: ac.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/utmp.h
ac: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/types.h
ac: /usr/include/sys/timeb.h

accton: accton.c

arff: arff.c /usr/include/sys/types.h /usr/include/sys/stat.h
arff: /usr/include/sys/time.h /usr/include/time.h /usr/include/signal.h
arff: /usr/include/stdio.h /usr/include/sys/file.h

bad144: bad144.c /usr/include/sys/types.h /usr/include/sys/dkbad.h
bad144: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
bad144: /usr/include/sys/ttydev.h /usr/include/sys/file.h
bad144: /usr/include/machine/dkio.h /usr/include/stdio.h /usr/include/disktab.h

badsect: badsect.c /usr/include/stdio.h /usr/include/sys/param.h
badsect: /usr/include/machine/machparam.h /usr/include/signal.h
badsect: /usr/include/sys/types.h /usr/include/sys/fs.h /usr/include/sys/dir.h
badsect: /usr/include/sys/stat.h /usr/include/sys/inode.h

catman: catman.c /usr/include/stdio.h /usr/include/sys/types.h
catman: /usr/include/sys/stat.h /usr/include/sys/file.h /usr/include/sys/time.h
catman: /usr/include/time.h /usr/include/sys/dir.h /usr/include/ctype.h

chown: chown.c /usr/include/stdio.h /usr/include/ctype.h
chown: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/pwd.h
chown: /usr/include/sys/dir.h /usr/include/grp.h /usr/include/strings.h

clri: clri.c /usr/include/sys/param.h /usr/include/machine/machparam.h
clri: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/inode.h
clri: /usr/include/sys/fs.h

comsat: comsat.c /usr/include/sys/types.h /usr/include/sys/socket.h
comsat: /usr/include/sys/stat.h /usr/include/sys/wait.h /usr/include/sys/file.h
comsat: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/sgtty.h
comsat: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
comsat: /usr/include/sys/ttydev.h /usr/include/utmp.h /usr/include/signal.h
comsat: /usr/include/errno.h /usr/include/netdb.h /usr/include/syslog.h

cron: cron.c /usr/include/sys/types.h /usr/include/stdio.h /usr/include/ctype.h
cron: /usr/include/signal.h /usr/include/sys/time.h /usr/include/time.h
cron: /usr/include/sys/stat.h /usr/include/sys/wait.h /usr/include/sys/ioctl.h
cron: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cron: /usr/include/sys/file.h /usr/include/pwd.h

dcheck: dcheck.c /usr/include/sys/param.h /usr/include/machine/machparam.h
dcheck: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/inode.h
dcheck: /usr/include/sys/fs.h /usr/include/sys/dir.h /usr/include/stdio.h

diskpart: diskpart.c /usr/include/sys/param.h /usr/include/machine/machparam.h
diskpart: /usr/include/signal.h /usr/include/sys/types.h /usr/include/stdio.h
diskpart: /usr/include/disktab.h

dumpfs: dumpfs.c /usr/include/sys/param.h /usr/include/machine/machparam.h
dumpfs: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/inode.h
dumpfs: /usr/include/sys/fs.h /usr/include/stdio.h /usr/include/fstab.h

edquota: edquota.c /usr/include/stdio.h /usr/include/signal.h
edquota: /usr/include/errno.h /usr/include/pwd.h /usr/include/ctype.h
edquota: /usr/include/fstab.h /usr/include/sys/param.h
edquota: /usr/include/machine/machparam.h /usr/include/signal.h
edquota: /usr/include/sys/types.h /usr/include/sys/stat.h
edquota: /usr/include/sys/file.h /usr/include/sys/quota.h

fingerd: fingerd.c /usr/include/sys/types.h /usr/include/netinet/in.h
fingerd: /usr/include/stdio.h /usr/include/ctype.h

flcopy: flcopy.c /usr/include/sys/file.h

gettable: gettable.c /usr/include/sys/types.h /usr/include/sys/socket.h
gettable: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/netdb.h

ifconfig: ifconfig.c /usr/include/sys/types.h /usr/include/sys/socket.h
ifconfig: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ifconfig: /usr/include/sys/ttydev.h /usr/include/net/if.h
ifconfig: /usr/include/net/if_arp.h /usr/include/netinet/in.h
ifconfig: /usr/include/netns/ns.h /usr/include/netns/ns_if.h
ifconfig: /usr/include/stdio.h /usr/include/errno.h /usr/include/ctype.h
ifconfig: /usr/include/netdb.h

halt: halt.c /usr/include/stdio.h /usr/include/sys/reboot.h
halt: /usr/include/sys/types.h /usr/include/sys/time.h /usr/include/time.h
halt: /usr/include/errno.h /usr/include/signal.h /usr/include/utmp.h

icheck: icheck.c /usr/include/stdio.h /usr/include/sys/param.h
icheck: /usr/include/machine/machparam.h /usr/include/signal.h
icheck: /usr/include/sys/types.h /usr/include/sys/inode.h /usr/include/sys/fs.h

inetd: inetd.c /usr/include/sys/param.h /usr/include/machine/machparam.h
inetd: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
inetd: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
inetd: /usr/include/sys/ttydev.h /usr/include/sys/socket.h
inetd: /usr/include/sys/file.h /usr/include/sys/wait.h /usr/include/sys/time.h
inetd: /usr/include/time.h /usr/include/sys/resource.h
inetd: /usr/include/netinet/in.h /usr/include/arpa/inet.h /usr/include/errno.h
inetd: /usr/include/stdio.h /usr/include/signal.h /usr/include/netdb.h
inetd: /usr/include/syslog.h /usr/include/pwd.h /usr/include/ctype.h

init: init.c /usr/include/signal.h /usr/include/sys/types.h /usr/include/utmp.h
init: /usr/include/setjmp.h /usr/include/sys/reboot.h /usr/include/errno.h
init: /usr/include/sys/file.h /usr/include/ttyent.h /usr/include/syslog.h
init: /usr/include/sys/stat.h /usr/include/sys/ioctl.h
init: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h

newfs: newfs.c /usr/include/sys/param.h /usr/include/machine/machparam.h
newfs: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
newfs: /usr/include/sys/fs.h /usr/include/sys/dir.h /usr/include/stdio.h
newfs: /usr/include/disktab.h

mkfs: mkfs.c /usr/include/stdio.h /usr/include/a.out.h /usr/include/sys/exec.h
mkfs: /usr/include/sys/param.h /usr/include/machine/machparam.h
mkfs: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/inode.h
mkfs: /usr/include/sys/fs.h /usr/include/sys/dir.h

mknod: mknod.c /usr/include/stdio.h

mkproto: mkproto.c /usr/include/stdio.h /usr/include/sys/param.h
mkproto: /usr/include/machine/machparam.h /usr/include/signal.h
mkproto: /usr/include/sys/types.h /usr/include/sys/inode.h
mkproto: /usr/include/sys/fs.h /usr/include/sys/dir.h

mount: mount.c /usr/include/sys/param.h /usr/include/machine/machparam.h
mount: /usr/include/signal.h /usr/include/sys/types.h /usr/include/stdio.h
mount: /usr/include/fstab.h /usr/include/mtab.h /usr/include/errno.h

ncheck: ncheck.c /usr/include/sys/param.h /usr/include/machine/machparam.h
ncheck: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/inode.h
ncheck: /usr/include/sys/fs.h /usr/include/sys/dir.h /usr/include/stdio.h

quot: quot.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/sys/param.h
quot: /usr/include/machine/machparam.h /usr/include/signal.h
quot: /usr/include/sys/types.h /usr/include/sys/inode.h /usr/include/sys/fs.h
quot: /usr/include/sys/file.h /usr/include/fstab.h /usr/include/pwd.h
quot: /usr/include/grp.h /usr/include/utmp.h

quotaon: quotaon.c /usr/include/sys/param.h /usr/include/machine/machparam.h
quotaon: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/file.h
quotaon: /usr/include/stdio.h /usr/include/fstab.h /usr/include/mtab.h

quotacheck: quotacheck.c /usr/include/stdio.h /usr/include/ctype.h
quotacheck: /usr/include/signal.h /usr/include/errno.h /usr/include/sys/param.h
quotacheck: /usr/include/machine/machparam.h /usr/include/signal.h
quotacheck: /usr/include/sys/types.h /usr/include/sys/inode.h
quotacheck: /usr/include/sys/fs.h /usr/include/sys/quota.h
quotacheck: /usr/include/sys/stat.h /usr/include/sys/wait.h
quotacheck: /usr/include/fstab.h /usr/include/pwd.h

reboot: reboot.c /usr/include/stdio.h /usr/include/sys/reboot.h
reboot: /usr/include/errno.h /usr/include/signal.h /usr/include/sys/types.h
reboot: /usr/include/sys/time.h /usr/include/time.h /usr/include/utmp.h

renice: renice.c /usr/include/sys/time.h /usr/include/time.h
renice: /usr/include/sys/resource.h /usr/include/stdio.h /usr/include/pwd.h

repquota: repquota.c /usr/include/stdio.h /usr/include/errno.h
repquota: /usr/include/sys/param.h /usr/include/machine/machparam.h
repquota: /usr/include/signal.h /usr/include/sys/types.h
repquota: /usr/include/sys/quota.h /usr/include/sys/stat.h /usr/include/fstab.h
repquota: /usr/include/pwd.h

rexecd: rexecd.c /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
rexecd: /usr/include/sys/ttydev.h /usr/include/sys/param.h
rexecd: /usr/include/machine/machparam.h /usr/include/signal.h
rexecd: /usr/include/sys/types.h /usr/include/sys/socket.h
rexecd: /usr/include/sys/wait.h /usr/include/netinet/in.h /usr/include/stdio.h
rexecd: /usr/include/errno.h /usr/include/pwd.h /usr/include/signal.h
rexecd: /usr/include/netdb.h

rlogind: rlogind.c /usr/include/stdio.h /usr/include/sys/types.h
rlogind: /usr/include/sys/stat.h /usr/include/sys/socket.h
rlogind: /usr/include/sys/wait.h /usr/include/sys/file.h
rlogind: /usr/include/netinet/in.h /usr/include/errno.h /usr/include/pwd.h
rlogind: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
rlogind: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
rlogind: /usr/include/stdio.h /usr/include/netdb.h /usr/include/syslog.h
rlogind: /usr/include/strings.h /usr/include/utmp.h

rmt: rmt.c /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
rmt: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
rmt: /usr/include/sys/types.h /usr/include/sys/socket.h /usr/include/sys/mtio.h
rmt: /usr/include/errno.h

route: route.c /usr/include/sys/types.h /usr/include/sys/socket.h
route: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
route: /usr/include/sys/ttydev.h /usr/include/sys/mbuf.h
route: /usr/include/net/route.h /usr/include/netinet/in.h /usr/include/stdio.h
route: /usr/include/errno.h /usr/include/ctype.h /usr/include/netdb.h
route: /usr/include/nlist.h

rshd: rshd.c /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
rshd: /usr/include/sys/ttydev.h /usr/include/sys/param.h
rshd: /usr/include/machine/machparam.h /usr/include/signal.h
rshd: /usr/include/sys/types.h /usr/include/sys/socket.h
rshd: /usr/include/sys/wait.h /usr/include/netinet/in.h
rshd: /usr/include/arpa/inet.h /usr/include/stdio.h /usr/include/errno.h
rshd: /usr/include/pwd.h /usr/include/signal.h /usr/include/netdb.h
rshd: /usr/include/syslog.h

rwhod: rwhod.c /usr/include/sys/types.h /usr/include/sys/socket.h
rwhod: /usr/include/sys/stat.h /usr/include/sys/ioctl.h
rwhod: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
rwhod: /usr/include/sys/file.h /usr/include/net/if.h /usr/include/net/if_arp.h
rwhod: /usr/include/netinet/in.h /usr/include/nlist.h /usr/include/stdio.h
rwhod: /usr/include/signal.h /usr/include/errno.h /usr/include/utmp.h
rwhod: /usr/include/ctype.h /usr/include/netdb.h /usr/include/syslog.h
rwhod: /usr/include/protocols/rwhod.h

rxformat: rxformat.c /usr/include/stdio.h /usr/include/sys/file.h
rxformat: /usr/include/errno.h /usr/include/vaxuba/rxreg.h
rxformat: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
rxformat: /usr/include/sys/ttydev.h

sa: sa.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/sys/types.h
sa: /usr/include/sys/acct.h /usr/include/signal.h /usr/include/utmp.h
sa: /usr/include/pwd.h

savecore: savecore.c /usr/include/stdio.h /usr/include/nlist.h
savecore: /usr/include/sys/param.h /usr/include/machine/machparam.h
savecore: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/dir.h
savecore: /usr/include/sys/stat.h /usr/include/sys/fs.h /usr/include/sys/time.h
savecore: /usr/include/time.h /usr/include/sys/file.h /usr/include/syslog.h

shutdown: shutdown.c /usr/include/stdio.h /usr/include/ctype.h
shutdown: /usr/include/signal.h /usr/include/setjmp.h /usr/include/utmp.h
shutdown: /usr/include/sys/time.h /usr/include/time.h
shutdown: /usr/include/sys/resource.h /usr/include/sys/types.h

swapon: swapon.c /usr/include/stdio.h /usr/include/fstab.h /usr/include/errno.h

syslogd: syslogd.c /usr/include/syslog.h /usr/include/errno.h
syslogd: /usr/include/stdio.h /usr/include/utmp.h /usr/include/ctype.h
syslogd: /usr/include/signal.h /usr/include/sysexits.h /usr/include/strings.h
syslogd: /usr/include/sys/types.h /usr/include/sys/param.h
syslogd: /usr/include/machine/machparam.h /usr/include/signal.h
syslogd: /usr/include/sys/types.h /usr/include/sys/ioctl.h
syslogd: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
syslogd: /usr/include/sys/stat.h /usr/include/sys/wait.h
syslogd: /usr/include/sys/socket.h /usr/include/sys/file.h
syslogd: /usr/include/sys/msgbuf.h /usr/include/sys/uio.h /usr/include/sys/un.h
syslogd: /usr/include/sys/time.h /usr/include/time.h
syslogd: /usr/include/sys/resource.h /usr/include/netinet/in.h
syslogd: /usr/include/netdb.h

telnetd: telnetd.c /usr/include/sys/types.h /usr/include/sys/socket.h
telnetd: /usr/include/sys/wait.h /usr/include/sys/file.h
telnetd: /usr/include/sys/stat.h /usr/include/netinet/in.h
telnetd: /usr/include/arpa/telnet.h /usr/include/stdio.h /usr/include/signal.h
telnetd: /usr/include/errno.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
telnetd: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
telnetd: /usr/include/netdb.h /usr/include/syslog.h /usr/include/utmp.h

tunefs: tunefs.c /usr/include/sys/param.h /usr/include/machine/machparam.h
tunefs: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
tunefs: /usr/include/sys/fs.h /usr/include/sys/inode.h /usr/include/stdio.h
tunefs: /usr/include/fstab.h

umount: umount.c /usr/include/sys/param.h /usr/include/machine/machparam.h
umount: /usr/include/signal.h /usr/include/sys/types.h /usr/include/stdio.h
umount: /usr/include/fstab.h /usr/include/mtab.h

update: update.c /usr/include/signal.h

vipw: vipw.c /usr/include/sys/types.h /usr/include/sys/stat.h
vipw: /usr/include/sys/file.h /usr/include/stdio.h /usr/include/errno.h
vipw: /usr/include/signal.h

mkpasswd: mkpasswd.c /usr/include/sys/file.h /usr/include/stdio.h
mkpasswd: /usr/include/pwd.h /usr/include/ndbm.h

pstat: pstat.c /usr/include/sys/param.h /usr/include/machine/machparam.h
pstat: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/dir.h
pstat: /usr/include/sys/file.h /usr/include/sys/user.h
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

arp: arp.c /usr/include/stdio.h /usr/include/sys/types.h
arp: /usr/include/sys/socket.h /usr/include/netinet/in.h
arp: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
arp: /usr/include/sys/ttydev.h /usr/include/errno.h /usr/include/netdb.h
arp: /usr/include/nlist.h /usr/include/net/if.h /usr/include/net/if_arp.h
arp: /usr/include/netinet/if_ether.h

dmesg: dmesg.c /usr/include/stdio.h /usr/include/sys/param.h
dmesg: /usr/include/machine/machparam.h /usr/include/signal.h
dmesg: /usr/include/sys/types.h /usr/include/nlist.h /usr/include/signal.h
dmesg: /usr/include/sys/file.h /usr/include/sys/vm.h /usr/include/sys/vmparam.h
dmesg: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
dmesg: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h
dmesg: /usr/include/sys/msgbuf.h

kgmon: kgmon.c /usr/include/sys/param.h /usr/include/machine/machparam.h
kgmon: /usr/include/signal.h /usr/include/sys/types.h
kgmon: /usr/include/machine/pte.h /usr/include/sys/file.h /usr/include/sys/vm.h
kgmon: /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
kgmon: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
kgmon: /usr/include/sys/vmsystm.h /usr/include/stdio.h /usr/include/nlist.h
kgmon: /usr/include/ctype.h /usr/include/sys/gprof.h

trpt: trpt.c /usr/include/sys/param.h /usr/include/machine/machparam.h
trpt: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/socket.h
trpt: /usr/include/sys/socketvar.h /usr/include/sys/protosw.h
trpt: /usr/include/net/route.h /usr/include/net/if.h /usr/include/net/if_arp.h
trpt: /usr/include/netinet/in.h /usr/include/netinet/in_pcb.h
trpt: /usr/include/netinet/in_systm.h /usr/include/netinet/ip.h
trpt: /usr/include/netinet/ip_var.h /usr/include/netinet/tcp.h
trpt: /usr/include/netinet/tcp_fsm.h /usr/include/netinet/tcp_seq.h
trpt: /usr/include/netinet/tcp_timer.h /usr/include/netinet/tcp_var.h
trpt: /usr/include/netinet/tcpip.h /usr/include/netinet/tcp_debug.h
trpt: /usr/include/arpa/inet.h /usr/include/stdio.h /usr/include/errno.h
trpt: /usr/include/nlist.h

ping: ping.c /usr/include/stdio.h /usr/include/errno.h /usr/include/sys/time.h
ping: /usr/include/time.h /usr/include/sys/param.h
ping: /usr/include/machine/machparam.h /usr/include/signal.h
ping: /usr/include/sys/types.h /usr/include/sys/socket.h
ping: /usr/include/sys/file.h /usr/include/netinet/in_systm.h
ping: /usr/include/netinet/in.h /usr/include/netinet/ip.h
ping: /usr/include/netinet/ip_icmp.h /usr/include/netdb.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
