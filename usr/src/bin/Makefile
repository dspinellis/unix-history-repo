#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.11 (Berkeley) %G%
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	adb as awk csh diff make sed sh tp

# Shell scripts that need only be installed and are never removed.
#
SCRIPT=	false true

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	ar cat cc chmod cmp date dd du echo ed grep hostid hostname \
	kill ld ln ls mkdir mt nice nm od pagesize pr pwd rm rmail \
	rmdir size stty sync tar tee test time wall who write

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	expr

# Programs that must run setuid to root
#
SETUID=	chgrp df login mail passwd rcp su

# Programs that must run set-group-id kmem.
#
KMEM=	ps

all:	${SUBDIR} ${STD} ${NSTD} ${SETUID} ${KMEM} cp mv strip

${SUBDIR}: /tmp
	cd $@; make ${MFLAGS}

${STD} ${SETUID} mv cp strip:
	${CC} ${CFLAGS} -o $@ $@.c

# take care with cp, strip, and mv, install uses them
install:
	install -s cp ${DESTDIR}/bin/newcp; \
		mv ${DESTDIR}/bin/newcp ${DESTDIR}/bin/cp;
	install -s strip ${DESTDIR}/bin/newstrip; \
		mv ${DESTDIR}/bin/newstrip ${DESTDIR}/bin/strip;
	rm -f ${DESTDIR}/bin/mv; cp mv ${DESTDIR}/bin/mv; rm -f mv; \
		strip ${DESTDIR}/bin/mv
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${SCRIPT}; do (install -c $$i.sh ${DESTDIR}/bin/$$i); done
	-for i in ${STD} ${NSTD}; do (install -s $$i ${DESTDIR}/bin/$$i); done
	-for i in ${SETUID}; do \
		(install -o root -m 4755 -s $$i ${DESTDIR}/bin/$$i); done
	-for i in ${KMEM}; do \
		(install -g kmem -m 2755 -s $$i ${DESTDIR}/bin/$$i); done
	rm -f ${DESTDIR}/bin/[; ln ${DESTDIR}/bin/test ${DESTDIR}/bin/[
	rm -f ${DESTDIR}/bin/e; ln ${DESTDIR}/bin/ed ${DESTDIR}/bin/e
	rm -f ${DESTDIR}/bin/chfn; ln ${DESTDIR}/bin/passwd ${DESTDIR}/bin/chfn
	rm -f ${DESTDIR}/bin/chsh; ln ${DESTDIR}/bin/passwd ${DESTDIR}/bin/chsh

clean:
	rm -f a.out core *.s *.o errs
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done
	rm -f ${STD} ${NSTD} ${KMEM} ${SETUID} expr.c cp mv strip

depend:
	for i in ${STD} ${NSTD} ${KMEM} ${SETUID} cp mv strip; do \
	    cc -M ${INCPATH} $$i.c | sed 's/\.o//' | \
	    awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
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

# Files listed in ${NSTD} have explicit make lines given below.

expr: expr.o
	cc ${CFLAGS} -o expr expr.o

ps:
	cc ${CFLAGS} -o ps ps.c -lm

# DO NOT DELETE THIS LINE -- make depend uses it

ar: ar.c /usr/include/sys/param.h /usr/include/machine/machparam.h
ar: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
ar: /usr/include/sys/time.h /usr/include/time.h /usr/include/stdio.h
ar: /usr/include/ar.h
cat: cat.c /usr/include/stdio.h /usr/include/sys/types.h
cat: /usr/include/sys/stat.h
cc: cc.c /usr/include/sys/param.h /usr/include/machine/machparam.h
cc: /usr/include/signal.h /usr/include/sys/types.h /usr/include/stdio.h
cc: /usr/include/ctype.h /usr/include/signal.h /usr/include/sys/dir.h
chmod: chmod.c /usr/include/stdio.h /usr/include/sys/types.h
chmod: /usr/include/sys/stat.h /usr/include/sys/dir.h
cmp: cmp.c /usr/include/stdio.h /usr/include/ctype.h
date: date.c /usr/include/sys/param.h /usr/include/machine/machparam.h
date: /usr/include/signal.h /usr/include/sys/types.h /usr/include/stdio.h
date: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/socket.h
date: /usr/include/netinet/in.h /usr/include/netdb.h
date: /usr/include/protocols/timed.h /usr/include/sys/file.h
date: /usr/include/errno.h /usr/include/syslog.h /usr/include/utmp.h
dd: dd.c /usr/include/stdio.h /usr/include/signal.h
du: du.c /usr/include/stdio.h /usr/include/sys/param.h
du: /usr/include/machine/machparam.h /usr/include/signal.h
du: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/sys/dir.h
echo: echo.c /usr/include/stdio.h
ed: ed.c /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ed: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h /usr/include/setjmp.h
grep: grep.c /usr/include/stdio.h /usr/include/ctype.h
hostid: hostid.c /usr/include/stdio.h
hostname: hostname.c /usr/include/stdio.h
kill: kill.c /usr/include/signal.h /usr/include/ctype.h
ld: ld.c /usr/include/sys/param.h /usr/include/machine/machparam.h
ld: /usr/include/signal.h /usr/include/sys/types.h /usr/include/signal.h
ld: /usr/include/stdio.h /usr/include/ctype.h /usr/include/ar.h
ld: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/ranlib.h
ld: /usr/include/sys/stat.h /usr/include/sys/file.h
ln: ln.c /usr/include/stdio.h /usr/include/sys/types.h /usr/include/sys/stat.h
ln: /usr/include/errno.h
ls: ls.c /usr/include/sys/param.h /usr/include/machine/machparam.h
ls: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
ls: /usr/include/sys/dir.h /usr/include/stdio.h /usr/include/sgtty.h
ls: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ls: /usr/include/sys/ttydev.h /usr/include/pwd.h /usr/include/grp.h
ls: /usr/include/utmp.h
mkdir: mkdir.c /usr/include/stdio.h
mt: mt.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/sys/types.h
mt: /usr/include/sys/mtio.h /usr/include/sys/ioctl.h
mt: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
mt: /usr/include/vaxmba/mtreg.h /usr/include/vaxmba/htreg.h
mt: /usr/include/vaxuba/utreg.h /usr/include/vaxuba/tmreg.h
mt: /usr/include/vaxuba/tsreg.h
nice: nice.c /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
nice: /usr/include/sys/resource.h
nm: nm.c /usr/include/sys/types.h /usr/include/ar.h /usr/include/stdio.h
nm: /usr/include/ctype.h /usr/include/a.out.h /usr/include/sys/exec.h
nm: /usr/include/stab.h /usr/include/sys/stat.h
od: od.c /usr/include/stdio.h /usr/include/sys/types.h /usr/include/sys/stat.h
pagesize: pagesize.c
pr: pr.c /usr/include/stdio.h /usr/include/signal.h /usr/include/sys/types.h
pr: /usr/include/sys/stat.h
pwd: pwd.c /usr/include/stdio.h /usr/include/sys/param.h
pwd: /usr/include/machine/machparam.h /usr/include/signal.h
pwd: /usr/include/sys/types.h
rm: rm.c /usr/include/stdio.h /usr/include/sys/param.h
rm: /usr/include/machine/machparam.h /usr/include/signal.h
rm: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/sys/dir.h
rm: /usr/include/sys/file.h
rmail: rmail.c /usr/include/stdio.h /usr/include/sysexits.h
rmdir: rmdir.c /usr/include/stdio.h
size: size.c /usr/include/stdio.h /usr/include/a.out.h /usr/include/sys/exec.h
stty: stty.c /usr/include/stdio.h /usr/include/sys/ioctl.h
stty: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
sync: sync.c
tar: tar.c /usr/include/stdio.h /usr/include/sys/param.h
tar: /usr/include/machine/machparam.h /usr/include/signal.h
tar: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/sys/dir.h
tar: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
tar: /usr/include/sys/ttydev.h /usr/include/sys/mtio.h /usr/include/sys/time.h
tar: /usr/include/time.h /usr/include/signal.h /usr/include/errno.h
tee: tee.c /usr/include/signal.h /usr/include/sys/types.h
tee: /usr/include/sys/stat.h /usr/include/errno.h
test: test.c /usr/include/stdio.h /usr/include/sys/types.h
test: /usr/include/sys/stat.h
time: time.c /usr/include/stdio.h /usr/include/signal.h
time: /usr/include/sys/types.h /usr/include/sys/time.h /usr/include/time.h
time: /usr/include/sys/resource.h
wall: wall.c /usr/include/stdio.h /usr/include/utmp.h /usr/include/errno.h
wall: /usr/include/signal.h /usr/include/sys/time.h /usr/include/time.h
wall: /usr/include/fcntl.h /usr/include/sys/types.h /usr/include/sys/stat.h
who: who.c /usr/include/stdio.h /usr/include/utmp.h /usr/include/pwd.h
who: /usr/include/ctype.h
write: write.c /usr/include/stdio.h /usr/include/ctype.h
write: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/signal.h
write: /usr/include/utmp.h /usr/include/sys/time.h /usr/include/time.h
expr: expr.c /usr/include/stdio.h
ps: ps.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/nlist.h
ps: /usr/include/pwd.h /usr/include/sys/param.h
ps: /usr/include/machine/machparam.h /usr/include/signal.h
ps: /usr/include/sys/types.h /usr/include/sys/ioctl.h
ps: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ps: /usr/include/sys/tty.h /usr/include/sys/ttychars.h
ps: /usr/include/sys/ttydev.h /usr/include/sys/dir.h /usr/include/sys/user.h
ps: /usr/include/machine/pcb.h /usr/include/sys/dmap.h /usr/include/sys/time.h
ps: /usr/include/time.h /usr/include/sys/resource.h /usr/include/sys/namei.h
ps: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/sys/proc.h
ps: /usr/include/machine/pte.h /usr/include/sys/vm.h /usr/include/sys/vmparam.h
ps: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
ps: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h
ps: /usr/include/sys/text.h /usr/include/sys/stat.h /usr/include/sys/mbuf.h
ps: /usr/include/math.h /usr/include/errno.h
chgrp: chgrp.c /usr/include/stdio.h /usr/include/ctype.h
chgrp: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/grp.h
chgrp: /usr/include/pwd.h /usr/include/sys/dir.h
df: df.c /usr/include/sys/param.h /usr/include/machine/machparam.h
df: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/fs.h
df: /usr/include/sys/stat.h /usr/include/errno.h /usr/include/stdio.h
df: /usr/include/fstab.h /usr/include/mtab.h
login: login.c /usr/include/sys/param.h /usr/include/machine/machparam.h
login: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/quota.h
login: /usr/include/sys/stat.h /usr/include/sys/time.h /usr/include/time.h
login: /usr/include/sys/resource.h /usr/include/sys/file.h /usr/include/sgtty.h
login: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
login: /usr/include/sys/ttydev.h /usr/include/utmp.h /usr/include/signal.h
login: /usr/include/pwd.h /usr/include/stdio.h /usr/include/lastlog.h
login: /usr/include/errno.h /usr/include/ttyent.h /usr/include/syslog.h
mail: mail.c /usr/include/sys/types.h /usr/include/sys/stat.h
mail: /usr/include/sys/file.h /usr/include/ctype.h /usr/include/stdio.h
mail: /usr/include/pwd.h /usr/include/utmp.h /usr/include/signal.h
mail: /usr/include/setjmp.h /usr/include/sysexits.h /usr/include/sys/socket.h
mail: /usr/include/netinet/in.h /usr/include/netdb.h
passwd: passwd.c /usr/include/sys/types.h /usr/include/sys/file.h
passwd: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/resource.h
passwd: /usr/include/stdio.h /usr/include/signal.h /usr/include/pwd.h
passwd: /usr/include/ndbm.h /usr/include/errno.h /usr/include/strings.h
passwd: /usr/include/ctype.h
rcp: rcp.c /usr/include/sys/param.h /usr/include/machine/machparam.h
rcp: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
rcp: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/ioctl.h
rcp: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
rcp: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/signal.h
rcp: /usr/include/pwd.h /usr/include/ctype.h /usr/include/netdb.h
rcp: /usr/include/errno.h /usr/include/sys/dir.h
su: su.c /usr/include/stdio.h /usr/include/pwd.h /usr/include/grp.h
su: /usr/include/syslog.h /usr/include/sys/types.h /usr/include/sys/time.h
su: /usr/include/time.h /usr/include/sys/resource.h
cp: cp.c /usr/include/stdio.h /usr/include/sys/param.h
cp: /usr/include/machine/machparam.h /usr/include/signal.h
cp: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/sys/dir.h
cp: /usr/include/sys/time.h /usr/include/time.h
mv: mv.c /usr/include/sys/param.h /usr/include/machine/machparam.h
mv: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
mv: /usr/include/sys/time.h /usr/include/time.h /usr/include/stdio.h
mv: /usr/include/sys/dir.h /usr/include/errno.h /usr/include/signal.h
strip: strip.c /usr/include/a.out.h /usr/include/sys/exec.h
strip: /usr/include/signal.h /usr/include/stdio.h /usr/include/sys/file.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
