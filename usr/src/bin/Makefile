#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.8 (Berkeley) %G%
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
STD=	ar cat cc chmod cmp date dd du echo ed expr grep hostid hostname \
	kill ld ln ls mkdir mt nice nm od pagesize pr pwd rm rmail \
	rmdir size strip stty sync tar tee test time wall who write

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	

# Programs that must run setuid to root
#
SETUID=	chgrp df login mail passwd rcp su

# Programs that must run set-group-id kmem.
#
KMEM=	ps

all:	${SUBDIR} ${STD} ${NSTD} ${SETUID} ${KMEM} cp mv

${SUBDIR}: /tmp
	cd $@; make ${MFLAGS}

${STD} ${SETUID} mv cp:
	${CC} ${CFLAGS} -o $@ $@.c

# take care with cp and mv, install uses them
install:
	install -s cp ${DESTDIR}/bin/newcp; \
		mv ${DESTDIR}/bin/newcp ${DESTDIR}/bin/cp;
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
	rm -f ${STD} ${NSTD} ${KMEM} ${SETUID} expr.c cp mv

depend:
	for i in ${STD} ${NSTD} ${KMEM} ${SETUID} cp mv; do \
	    cc -M ${INCPATH} $$i.c | \
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

# Files listed in ${NSTD} have explicit make lines given below.

ps:
	cc ${CFLAGS} -o ps ps.c -lm

# DO NOT DELETE THIS LINE -- make depend uses it


ar.o: ar.c /usr/include/sys/param.h /usr/include/machine/machparam.h
ar.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
ar.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/stdio.h
ar.o: /usr/include/ar.h

cat.o: cat.c /usr/include/stdio.h /usr/include/sys/types.h
cat.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
cat.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h

cc.o: cc.c /usr/include/sys/param.h /usr/include/machine/machparam.h
cc.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/stdio.h
cc.o: /usr/include/ctype.h /usr/include/signal.h /usr/include/sys/dir.h

chmod.o: chmod.c /usr/include/stdio.h /usr/include/sys/types.h
chmod.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
chmod.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
chmod.o: /usr/include/sys/dir.h

cmp.o: cmp.c /usr/include/stdio.h /usr/include/ctype.h

date.o: date.c /usr/include/sys/types.h /usr/include/sys/param.h
date.o: /usr/include/machine/machparam.h /usr/include/signal.h
date.o: /usr/include/sys/types.h /usr/include/stdio.h /usr/include/sys/time.h
date.o: /usr/include/time.h /usr/include/sys/socket.h /usr/include/netinet/in.h
date.o: /usr/include/netdb.h /usr/include/protocols/timed.h
date.o: /usr/include/sys/file.h /usr/include/errno.h /usr/include/syslog.h
date.o: /usr/include/utmp.h

dd.o: dd.c /usr/include/stdio.h /usr/include/signal.h

du.o: du.c /usr/include/stdio.h /usr/include/sys/param.h
du.o: /usr/include/machine/machparam.h /usr/include/signal.h
du.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/sys/dir.h

echo.o: echo.c /usr/include/stdio.h

ed.o: ed.c /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ed.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ed.o: /usr/include/setjmp.h

expr.o: expr.c /usr/include/stdio.h

grep.o: grep.c /usr/include/stdio.h /usr/include/ctype.h

hostid.o: hostid.c /usr/include/stdio.h

hostname.o: hostname.c /usr/include/stdio.h

kill.o: kill.c /usr/include/signal.h /usr/include/ctype.h

ld.o: ld.c /usr/include/sys/param.h /usr/include/machine/machparam.h
ld.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/signal.h
ld.o: /usr/include/stdio.h /usr/include/ctype.h /usr/include/ar.h
ld.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/ranlib.h
ld.o: /usr/include/sys/stat.h /usr/include/sys/file.h

ln.o: ln.c /usr/include/stdio.h /usr/include/sys/types.h
ln.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
ln.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
ln.o: /usr/include/errno.h

ls.o: ls.c /usr/include/sys/param.h /usr/include/machine/machparam.h
ls.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
ls.o: /usr/include/sys/dir.h /usr/include/stdio.h /usr/include/sgtty.h
ls.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ls.o: /usr/include/sys/ttydev.h /usr/include/pwd.h /usr/include/grp.h
ls.o: /usr/include/utmp.h

mkdir.o: mkdir.c /usr/include/stdio.h

mt.o: mt.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/sys/types.h
mt.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
mt.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/mtio.h
mt.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
mt.o: /usr/include/sys/ttydev.h /usr/include/vaxmba/mtreg.h
mt.o: /usr/include/vaxmba/htreg.h /usr/include/vaxuba/utreg.h
mt.o: /usr/include/vaxuba/tmreg.h /usr/include/vaxuba/tsreg.h

nice.o: nice.c /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
nice.o: /usr/include/sys/resource.h

nm.o: nm.c /usr/include/sys/types.h /usr/include/sys/param.h
nm.o: /usr/include/machine/machparam.h /usr/include/signal.h
nm.o: /usr/include/sys/types.h /usr/include/ar.h /usr/include/stdio.h
nm.o: /usr/include/ctype.h /usr/include/a.out.h /usr/include/sys/exec.h
nm.o: /usr/include/stab.h /usr/include/sys/stat.h

od.o: od.c /usr/include/stdio.h /usr/include/sys/types.h
od.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
od.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h

pagesize.o: pagesize.c

pr.o: pr.c /usr/include/stdio.h /usr/include/signal.h /usr/include/sys/types.h
pr.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
pr.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h

pwd.o: pwd.c /usr/include/stdio.h /usr/include/sys/param.h
pwd.o: /usr/include/machine/machparam.h /usr/include/signal.h
pwd.o: /usr/include/sys/types.h

rm.o: rm.c /usr/include/stdio.h /usr/include/sys/param.h
rm.o: /usr/include/machine/machparam.h /usr/include/signal.h
rm.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/sys/dir.h
rm.o: /usr/include/sys/file.h

rmail.o: rmail.c /usr/include/stdio.h /usr/include/sysexits.h

rmdir.o: rmdir.c /usr/include/stdio.h

size.o: size.c /usr/include/stdio.h /usr/include/a.out.h
size.o: /usr/include/sys/exec.h

strip.o: strip.c /usr/include/a.out.h /usr/include/sys/exec.h
strip.o: /usr/include/signal.h /usr/include/stdio.h /usr/include/sys/file.h

stty.o: stty.c /usr/include/stdio.h /usr/include/sys/ioctl.h
stty.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h

sync.o: sync.c

tar.o: tar.c /usr/include/stdio.h /usr/include/sys/param.h
tar.o: /usr/include/machine/machparam.h /usr/include/signal.h
tar.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/sys/dir.h
tar.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
tar.o: /usr/include/sys/ttydev.h /usr/include/sys/mtio.h
tar.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/signal.h
tar.o: /usr/include/errno.h

tee.o: tee.c /usr/include/signal.h /usr/include/sys/types.h
tee.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
tee.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
tee.o: /usr/include/errno.h

test.o: test.c /usr/include/stdio.h /usr/include/sys/types.h
test.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
test.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h

time.o: time.c /usr/include/stdio.h /usr/include/signal.h
time.o: /usr/include/sys/types.h /usr/include/sys/param.h
time.o: /usr/include/machine/machparam.h /usr/include/signal.h
time.o: /usr/include/sys/types.h /usr/include/sys/time.h /usr/include/time.h
time.o: /usr/include/sys/resource.h

wall.o: wall.c /usr/include/stdio.h /usr/include/utmp.h /usr/include/errno.h
wall.o: /usr/include/signal.h /usr/include/sys/time.h /usr/include/time.h
wall.o: /usr/include/fcntl.h /usr/include/sys/types.h /usr/include/sys/param.h
wall.o: /usr/include/machine/machparam.h /usr/include/signal.h
wall.o: /usr/include/sys/types.h /usr/include/sys/stat.h

who.o: who.c /usr/include/stdio.h /usr/include/utmp.h /usr/include/pwd.h
who.o: /usr/include/ctype.h

write.o: write.c /usr/include/stdio.h /usr/include/ctype.h
write.o: /usr/include/sys/types.h /usr/include/sys/param.h
write.o: /usr/include/machine/machparam.h /usr/include/signal.h
write.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/signal.h
write.o: /usr/include/utmp.h /usr/include/sys/time.h /usr/include/time.h

ps.o: ps.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/nlist.h
ps.o: /usr/include/pwd.h /usr/include/sys/param.h
ps.o: /usr/include/machine/machparam.h /usr/include/signal.h
ps.o: /usr/include/sys/types.h /usr/include/sys/ioctl.h
ps.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ps.o: /usr/include/sys/tty.h /usr/include/sys/ttychars.h
ps.o: /usr/include/sys/ttydev.h /usr/include/sys/dir.h /usr/include/sys/user.h
ps.o: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
ps.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/resource.h
ps.o: /usr/include/sys/namei.h /usr/include/sys/uio.h /usr/include/errno.h
ps.o: /usr/include/sys/proc.h /usr/include/machine/pte.h /usr/include/sys/vm.h
ps.o: /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
ps.o: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
ps.o: /usr/include/sys/vmsystm.h /usr/include/sys/text.h
ps.o: /usr/include/sys/stat.h /usr/include/sys/mbuf.h /usr/include/math.h
ps.o: /usr/include/errno.h

chgrp.o: chgrp.c /usr/include/stdio.h /usr/include/ctype.h
chgrp.o: /usr/include/sys/types.h /usr/include/sys/param.h
chgrp.o: /usr/include/machine/machparam.h /usr/include/signal.h
chgrp.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/grp.h
chgrp.o: /usr/include/pwd.h /usr/include/sys/dir.h

df.o: df.c /usr/include/sys/param.h /usr/include/machine/machparam.h
df.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/fs.h
df.o: /usr/include/sys/stat.h /usr/include/errno.h /usr/include/stdio.h
df.o: /usr/include/fstab.h /usr/include/mtab.h

login.o: login.c /usr/include/sys/param.h /usr/include/machine/machparam.h
login.o: /usr/include/signal.h /usr/include/sys/types.h
login.o: /usr/include/sys/quota.h /usr/include/sys/stat.h
login.o: /usr/include/sys/time.h /usr/include/time.h
login.o: /usr/include/sys/resource.h /usr/include/sys/file.h
login.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
login.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
login.o: /usr/include/utmp.h /usr/include/signal.h /usr/include/pwd.h
login.o: /usr/include/stdio.h /usr/include/lastlog.h /usr/include/errno.h
login.o: /usr/include/ttyent.h /usr/include/syslog.h

mail.o: mail.c /usr/include/sys/types.h /usr/include/sys/param.h
mail.o: /usr/include/machine/machparam.h /usr/include/signal.h
mail.o: /usr/include/sys/types.h /usr/include/sys/stat.h
mail.o: /usr/include/sys/file.h /usr/include/ctype.h /usr/include/stdio.h
mail.o: /usr/include/pwd.h /usr/include/utmp.h /usr/include/signal.h
mail.o: /usr/include/setjmp.h /usr/include/sysexits.h /usr/include/sys/socket.h
mail.o: /usr/include/netinet/in.h /usr/include/netdb.h

passwd.o: passwd.c /usr/include/sys/types.h /usr/include/sys/param.h
passwd.o: /usr/include/machine/machparam.h /usr/include/signal.h
passwd.o: /usr/include/sys/types.h /usr/include/sys/file.h
passwd.o: /usr/include/sys/time.h /usr/include/time.h
passwd.o: /usr/include/sys/resource.h /usr/include/stdio.h
passwd.o: /usr/include/signal.h /usr/include/pwd.h /usr/include/ndbm.h
passwd.o: /usr/include/errno.h /usr/include/strings.h /usr/include/ctype.h

rcp.o: rcp.c /usr/include/sys/param.h /usr/include/machine/machparam.h
rcp.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
rcp.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/ioctl.h
rcp.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
rcp.o: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/signal.h
rcp.o: /usr/include/pwd.h /usr/include/ctype.h /usr/include/netdb.h
rcp.o: /usr/include/errno.h /usr/include/sys/dir.h

su.o: su.c /usr/include/stdio.h /usr/include/pwd.h /usr/include/grp.h
su.o: /usr/include/syslog.h /usr/include/sys/types.h /usr/include/sys/param.h
su.o: /usr/include/machine/machparam.h /usr/include/signal.h
su.o: /usr/include/sys/types.h /usr/include/sys/time.h /usr/include/time.h
su.o: /usr/include/sys/resource.h

cp.o: cp.c /usr/include/stdio.h /usr/include/sys/param.h
cp.o: /usr/include/machine/machparam.h /usr/include/signal.h
cp.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/sys/dir.h
cp.o: /usr/include/sys/time.h /usr/include/time.h

mv.o: mv.c /usr/include/sys/param.h /usr/include/machine/machparam.h
mv.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
mv.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/stdio.h
mv.o: /usr/include/sys/dir.h /usr/include/errno.h /usr/include/signal.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
