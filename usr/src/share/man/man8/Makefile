#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#
MDIR=	/usr/man/cat8
SRCS=	XNSrouted.8 \
	ac.8 adduser.8 arp.8 badsect.8 bugfiler.8 chown.8 \
	clri.8 comsat.8 config.8 cron.8 dcheck.8 disklabel.8 diskpart.8 \
	dm.8 dmesg.8 dump.8 dumpfs.8 edquota.8 fastboot.8 fingerd.8 \
	fsck.8 fsdb.8 fstat.8 ftpd.8 gettable.8 getty.8 halt.8 \
	htable.8 icheck.8 ifconfig.8 implog.8 implogd.8 inetd.8 init.8 \
	intro.8 kgmon.8 lpc.8 lpd.8 makedev.8 makekey.8 mkhosts.8 \
	mklost+found.8 mknod.8 mkpasswd.8 mkproto.8 mount.8 named.8 \
	ncheck.8 newfs.8 pac.8 ping.8 pstat.8 quot.8 quotacheck.8 quotaon.8 \
	rc.8 rdump.8 renice.8 repquota.8 restore.8 rexecd.8 \
	rlogind.8 rmt.8 route.8 routed.8 rrestore.8 rshd.8 rwhod.8 sa.8 \
	savecore.8 sendmail.8 shutdown.8 slattach.8 sticky.8 swapon.8 \
	sync.8 syslogd.8 talkd.8 telnetd.8 tftpd.8 timed.8 timedc.8 trpt.8 \
	trsp.8 tunefs.8 update.8 uucico.8 uuclean.8 uupoll.8 uusnap.8 \
	uuxqt.8 vipw.8
OBJS=	XNSrouted.0 \
	ac.0 adduser.0 arp.0 badsect.0 bugfiler.0 chown.0 \
	clri.0 comsat.0 config.0 cron.0 dcheck.0 disklabel.0 diskpart.0 \
	dm.0 dmesg.0 dump.0 dumpfs.0 edquota.0 fastboot.0 fingerd.0 \
	fsck.0 fsdb.0 fstat.0 ftpd.0 gettable.0 getty.0 halt.0 \
	htable.0 icheck.0 ifconfig.0 implog.0 implogd.0 inetd.0 init.0 \
	intro.0 kgmon.0 lpc.0 lpd.0 makedev.0 makekey.0 mkhosts.0 \
	mklost+found.0 mknod.0 mkpasswd.0 mkproto.0 mount.0 named.0 \
	ncheck.0 newfs.0 pac.0 ping.0 pstat.0 quot.0 quotacheck.0 quotaon.0 \
	rc.0 rdump.0 renice.0 repquota.0 restore.0 rexecd.0 \
	rlogind.0 rmt.0 route.0 routed.0 rrestore.0 rshd.0 rwhod.0 sa.0 \
	savecore.0 sendmail.0 shutdown.0 slattach.0 sticky.0 swapon.0 \
	sync.0 syslogd.0 talkd.0 telnetd.0 tftpd.0 timed.0 timedc.0 trpt.0 \
	trsp.0 tunefs.0 update.0 uucico.0 uuclean.0 uupoll.0 uusnap.0 \
	uuxqt.0 vipw.0
LINKS=	accton.0 umount.0 mkfs.0 quotaoff.0 uucpd.0 fasthalt.0

.SUFFIXES: .8 .0

.8.0:
	${DESTDIR}/usr/man/manroff $*.8 > $*.0

all: ${OBJS}

clean: FRC
	rm -f ${OBJS}

install: FRC
	cd ${DESTDIR}${MDIR}; rm -f *.0
	install -c -o bin -g bin -m 444 ${OBJS} ${DESTDIR}${MDIR}
	ln ${DESTDIR}${MDIR}/sa.0 ${DESTDIR}${MDIR}/accton.0
	ln ${DESTDIR}${MDIR}/mount.0 ${DESTDIR}${MDIR}/umount.0
	ln ${DESTDIR}${MDIR}/newfs.0 ${DESTDIR}${MDIR}/mkfs.0
	ln ${DESTDIR}${MDIR}/quotaon.0 ${DESTDIR}${MDIR}/quotaoff.0
	ln ${DESTDIR}${MDIR}/uucico.0 ${DESTDIR}${MDIR}/uucpd.0
	ln ${DESTDIR}${MDIR}/fastboot.0 ${DESTDIR}${MDIR}/fasthalt.0

FRC:
