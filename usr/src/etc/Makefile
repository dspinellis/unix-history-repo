#	@(#)Makefile	4.15	(Berkeley)	2/26/83
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	config dump fsck restor termcap

# Shell scripts that need only be installed and are never removed.
#
SCRIPT=	fastboot mklost+found vipw

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	ac accton analyze arff bad144 badsect catman chown clri cron \
	dcheck diskpart dmesg dumpfs flcopy getty halt icheck init kgmon \
	newfs mkfs mknod mkproto mount ncheck pstat quot reboot \
	renice sa savecore shutdown swapon tunefs umount update

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	comsat

all:	${SUBDIR} ${STD} ${NSTD}

${SUBDIR}: /tmp
	cd $@; make ${MFLAGS}

${STD}:
	cc ${CFLAGS} -o $@ $@.c

install:
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	for i in ${SCRIPT}; do (install -c $$i.sh ${DESTDIR}/etc/$$i); done
	for i in ${STD} ${NSTD}; do (install $$i ${DESTDIR}/etc/$$i); done

clean:
	rm -f a.out core *.s *.o
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done
	rm -f ${STD} ${NSTD}

depend:
	cat </dev/null >x.c
	for i in ${STD} ${NSTD}; do \
		(echo $$i: $$i.c >>makedep; \
		/bin/grep '^#[ 	]*include' x.c $$i.c | sed \
			-e '/\.\.\/h/d' \
			-e 's,<\(.*\)>,"/usr/include/\1",' \
			-e 's/:[^"]*"\([^"]*\)".*/: \1/' \
			-e 's/\.c//' >>makedep); done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep x.c
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# Files listed in ${NSTD} have explicit make lines given below.

comsat:
	cc ${CFLAGS} -o comsat comsat.c -ljobs

# DO NOT DELETE THIS LINE -- make depend uses it

ac: ac.c
ac: /usr/include/stdio.h
ac: /usr/include/ctype.h
ac: /usr/include/time.h
ac: /usr/include/utmp.h
ac: /usr/include/sys/types.h
ac: /usr/include/sys/timeb.h
accton: accton.c
analyze: analyze.c
analyze: /usr/include/stdio.h
analyze: /usr/include/sys/param.h
analyze: /usr/include/sys/dir.h
analyze: /usr/include/machine/pte.h
analyze: /usr/include/nlist.h
analyze: /usr/include/sys/map.h
analyze: /usr/include/sys/user.h
analyze: /usr/include/sys/proc.h
analyze: /usr/include/sys/text.h
analyze: /usr/include/sys/cmap.h
analyze: /usr/include/sys/vm.h
arff: arff.c
arff: /usr/include/sys/types.h
arff: /usr/include/sys/stat.h
arff: /usr/include/time.h
arff: /usr/include/signal.h
arff: /usr/include/stdio.h
bad144: bad144.c
bad144: /usr/include/sys/types.h
bad144: /usr/include/sys/dkbad.h
bad144: /usr/include/sys/ioctl.h
bad144: /usr/include/machine/dkio.h
bad144: /usr/include/stdio.h
bad144: /usr/include/disktab.h
badsect: badsect.c
badsect: /usr/include/stdio.h
badsect: /usr/include/sys/param.h
badsect: /usr/include/sys/fs.h
badsect: /usr/include/sys/dir.h
badsect: /usr/include/sys/stat.h
badsect: /usr/include/sys/inode.h
catman: catman.c
catman: /usr/include/stdio.h
catman: /usr/include/sys/param.h
catman: /usr/include/stat.h
catman: /usr/include/dir.h
catman: /usr/include/ctype.h
chown: chown.c
chown: /usr/include/stdio.h
chown: /usr/include/ctype.h
chown: /usr/include/sys/types.h
chown: /usr/include/sys/stat.h
chown: /usr/include/pwd.h
clri: clri.c
clri: /usr/include/sys/param.h
clri: /usr/include/sys/inode.h
clri: /usr/include/sys/fs.h
cron: cron.c
cron: /usr/include/sys/types.h
cron: /usr/include/stdio.h
cron: /usr/include/ctype.h
cron: /usr/include/signal.h
cron: /usr/include/time.h
cron: /usr/include/sys/stat.h
dcheck: dcheck.c
dcheck: /usr/include/sys/param.h
dcheck: /usr/include/sys/inode.h
dcheck: /usr/include/sys/fs.h
dcheck: /usr/include/dir.h
dcheck: /usr/include/stdio.h
diskpart: diskpart.c
diskpart: /usr/include/sys/param.h
diskpart: /usr/include/stdio.h
diskpart: /usr/include/disktab.h
dmesg: dmesg.c
dmesg: /usr/include/stdio.h
dmesg: /usr/include/sys/param.h
dmesg: /usr/include/nlist.h
dmesg: /usr/include/signal.h
dmesg: /usr/include/sys/vm.h
dmesg: /usr/include/sys/msgbuf.h
dumpfs: dumpfs.c
dumpfs: /usr/include/sys/param.h
dumpfs: /usr/include/sys/inode.h
dumpfs: /usr/include/sys/fs.h
flcopy: flcopy.c
getty: getty.c
getty: /usr/include/sgtty.h
getty: /usr/include/signal.h
getty: /usr/include/ctype.h
halt: halt.c
halt: /usr/include/stdio.h
halt: /usr/include/sys/reboot.h
halt: /usr/include/sys/types.h
halt: /usr/include/time.h
halt: /usr/include/errno.h
halt: /usr/include/signal.h
halt: /usr/include/utmp.h
icheck: icheck.c
icheck: /usr/include/stdio.h
icheck: /usr/include/sys/param.h
icheck: /usr/include/sys/inode.h
icheck: /usr/include/sys/fs.h
init: init.c
init: /usr/include/signal.h
init: /usr/include/sys/types.h
init: /usr/include/utmp.h
init: /usr/include/setjmp.h
init: /usr/include/sys/reboot.h
init: /usr/include/errno.h
init: /usr/include/sys/ioctl.h
kgmon: kgmon.c
kgmon: /usr/include/sys/param.h
kgmon: /usr/include/machine/pte.h
kgmon: /usr/include/stdio.h
kgmon: /usr/include/nlist.h
kgmon: /usr/include/ctype.h
kgmon: /usr/include/sys/gprof.h
newfs: newfs.c
newfs: /usr/include/sys/param.h
newfs: /usr/include/sys/stat.h
newfs: /usr/include/sys/fs.h
newfs: /usr/include/stdio.h
newfs: /usr/include/dir.h
newfs: /usr/include/disktab.h
mkfs: mkfs.c
mkfs: /usr/include/stdio.h
mkfs: /usr/include/a.out.h
mkfs: /usr/include/sys/param.h
mkfs: /usr/include/sys/inode.h
mkfs: /usr/include/sys/fs.h
mkfs: /usr/include/dir.h
mknod: mknod.c
mkproto: mkproto.c
mkproto: /usr/include/stdio.h
mkproto: /usr/include/sys/param.h
mkproto: /usr/include/sys/inode.h
mkproto: /usr/include/sys/fs.h
mkproto: /usr/include/sys/dir.h
mount: mount.c
mount: /usr/include/stdio.h
mount: /usr/include/fstab.h
ncheck: ncheck.c
ncheck: /usr/include/sys/param.h
ncheck: /usr/include/sys/inode.h
ncheck: /usr/include/sys/fs.h
ncheck: /usr/include/stdio.h
ncheck: /usr/include/dir.h
pstat: pstat.c
pstat: /usr/include/sys/param.h
pstat: /usr/include/sys/dir.h
pstat: /usr/include/sys/file.h
pstat: /usr/include/sys/user.h
pstat: /usr/include/sys/proc.h
pstat: /usr/include/sys/text.h
pstat: /usr/include/sys/inode.h
pstat: /usr/include/sys/map.h
pstat: /usr/include/sys/ioctl.h
pstat: /usr/include/sys/tty.h
pstat: /usr/include/sys/conf.h
pstat: /usr/include/sys/vm.h
pstat: /usr/include/nlist.h
pstat: /usr/include/machine/pte.h
pstat: /usr/include/sys/descrip.h
quot: quot.c
quot: /usr/include/stdio.h
quot: /usr/include/ctype.h
quot: /usr/include/pwd.h
quot: /usr/include/sys/param.h
quot: /usr/include/sys/inode.h
quot: /usr/include/sys/fs.h
reboot: reboot.c
reboot: /usr/include/stdio.h
reboot: /usr/include/sys/reboot.h
reboot: /usr/include/errno.h
reboot: /usr/include/signal.h
reboot: /usr/include/sys/types.h
reboot: /usr/include/time.h
reboot: /usr/include/utmp.h
renice: renice.c
renice: /usr/include/sys/param.h
renice: /usr/include/sys/dir.h
renice: /usr/include/sys/user.h
renice: /usr/include/sys/proc.h
renice: /usr/include/nlist.h
renice: /usr/include/stdio.h
sa: sa.c
sa: /usr/include/stdio.h
sa: /usr/include/sys/types.h
sa: /usr/include/sys/acct.h
sa: /usr/include/signal.h
sa: /usr/include/utmp.h
sa: /usr/include/pwd.h
savecore: savecore.c
savecore: /usr/include/stdio.h
savecore: /usr/include/nlist.h
savecore: /usr/include/sys/param.h
savecore: /usr/include/sys/dir.h
savecore: /usr/include/sys/stat.h
savecore: /usr/include/sys/fs.h
savecore: /usr/include/time.h
shutdown: shutdown.c
shutdown: /usr/include/stdio.h
shutdown: /usr/include/ctype.h
shutdown: /usr/include/signal.h
shutdown: /usr/include/utmp.h
shutdown: /usr/include/time.h
shutdown: /usr/include/sys/types.h
swapon: swapon.c
swapon: /usr/include/stdio.h
swapon: /usr/include/fstab.h
tunefs: tunefs.c
tunefs: /usr/include/stdio.h
tunefs: /usr/include/sys/param.h
tunefs: /usr/include/sys/stat.h
tunefs: /usr/include/sys/fs.h
tunefs: /usr/include/sys/inode.h
umount: umount.c
umount: /usr/include/stdio.h
umount: /usr/include/fstab.h
update: update.c
update: /usr/include/signal.h
comsat: comsat.c
comsat: /usr/include/sys/types.h
comsat: /usr/include/sys/socket.h
comsat: /usr/include/netinet/in.h
comsat: /usr/include/stdio.h
comsat: /usr/include/sgtty.h
comsat: /usr/include/utmp.h
comsat: /usr/include/stat.h
comsat: /usr/include/wait.h
comsat: /usr/include/signal.h
comsat: /usr/include/errno.h
comsat: /usr/include/netdb.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
