#	@(#)Makefile	4.10	(Berkeley)	%G%
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	adb as awk csh diff make sed sh spell tp

# Shell scripts that need only be installed and are never removed.
#
SCRIPT=	false true

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	ar cat cc chgrp chmod cmp date dd df du echo ed expr \
	grep hostid hostname kill ld ln login ls mail mkdir mt nice \
	nm od pagesize passwd pr pwd rm rmail rmdir size strip stty su sync \
	tar tee test wall who write

# Programs that must run setuid to root
#
SETUID=	chgrp df login mail passwd su

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	ps time

all:	${SUBDIR} ${STD} ${NSTD} cp

${SUBDIR}: /tmp
	cd $@; make ${MFLAGS}

${STD} cp mv:
	${CC} ${CFLAGS} -o $@ $@.c

# take care with cp and mv, install uses them
install:
	install cp ${DESTDIR}/bin/newcp; \
		mv ${DESTDIR}/bin/newcp ${DESTDIR}/bin/cp
	install -c mv ${DESTDIR}/bin/mv; rm -f mv
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	for i in ${SCRIPT}; do (install -c $$i.sh ${DESTDIR}/bin/$$i); done
	for i in ${STD} ${NSTD}; do (install $$i ${DESTDIR}/bin/$$i); done
	for i in ${SETUID}; do (chown root ${DESTDIR}/bin/$$i; \
		chmod 4755 ${DESTDIR}/bin/$$i); done
	rm -f ${DESTDIR}/bin/[; ln ${DESTDIR}/bin/test ${DESTDIR}/bin/[
	rm -f ${DESTDIR}/bin/e; ln ${DESTDIR}/bin/ed ${DESTDIR}/bin/e

clean:
	rm -f a.out core *.s *.o errs
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done
	rm -f ${STD} ${NSTD} expr.c

depend:
	cat </dev/null >x.c
	for i in ${STD} ${NSTD}; do \
		(echo $$i: $$i.c >>makedep; \
		/bin/grep '^#[ 	]*include' x.c $$i.c | sed \
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

ps:
	cc ${CFLAGS} -o ps ps.c -lm

time:
	cc ${CFLAGS} -o time time.c -ljobs

# DO NOT DELETE THIS LINE -- make depend uses it

ar: ar.c
ar: /usr/include/stdio.h
ar: /usr/include/sys/types.h
ar: /usr/include/sys/stat.h
ar: /usr/include/ar.h
ar: /usr/include/signal.h
cat: cat.c
cat: /usr/include/stdio.h
cat: /usr/include/sys/types.h
cat: /usr/include/sys/stat.h
cc: cc.c
cc: /usr/include/sys/param.h
cc: /usr/include/stdio.h
cc: /usr/include/ctype.h
cc: /usr/include/signal.h
cc: /usr/include/dir.h
chgrp: chgrp.c
chgrp: /usr/include/stdio.h
chgrp: /usr/include/ctype.h
chgrp: /usr/include/sys/types.h
chgrp: /usr/include/sys/stat.h
chgrp: /usr/include/grp.h
chgrp: /usr/include/pwd.h
chmod: chmod.c
chmod: /usr/include/stdio.h
chmod: /usr/include/sys/types.h
chmod: /usr/include/sys/stat.h
cmp: cmp.c
cmp: /usr/include/stdio.h
cmp: /usr/include/ctype.h
cp: cp.c
cp: /usr/include/stdio.h
cp: /usr/include/sys/param.h
cp: /usr/include/sys/stat.h
cp: /usr/include/dir.h
date: date.c
date: /usr/include/stdio.h
date: /usr/include/time.h
date: /usr/include/sys/types.h
date: /usr/include/sys/timeb.h
date: /usr/include/utmp.h
dd: dd.c
dd: /usr/include/stdio.h
dd: /usr/include/signal.h
df: df.c
df: /usr/include/stdio.h
df: /usr/include/fstab.h
df: /usr/include/sys/param.h
df: /usr/include/sys/fs.h
df: /usr/include/sys/stat.h
du: du.c
du: /usr/include/stdio.h
du: /usr/include/sys/param.h
du: /usr/include/sys/stat.h
du: /usr/include/dir.h
echo: echo.c
echo: /usr/include/stdio.h
ed: ed.c
ed: /usr/include/signal.h
ed: /usr/include/sgtty.h
ed: /usr/include/setjmp.h
expr: expr.c
expr: /usr/include/stdio.h
grep: grep.c
grep: /usr/include/stdio.h
grep: /usr/include/ctype.h
hostname: hostname.c
hostname: /usr/include/stdio.h
kill: kill.c
kill: /usr/include/signal.h
kill: /usr/include/ctype.h
ld: ld.c
ld: /usr/include/sys/types.h
ld: /usr/include/signal.h
ld: /usr/include/stdio.h
ld: /usr/include/ctype.h
ld: /usr/include/ar.h
ld: /usr/include/a.out.h
ld: /usr/include/ranlib.h
ld: /usr/include/stat.h
ld: /usr/include/pagsiz.h
ln: ln.c
ln: /usr/include/stdio.h
ln: /usr/include/sys/types.h
ln: /usr/include/sys/stat.h
login: login.c
login: /usr/include/sys/types.h
login: /usr/include/sgtty.h
login: /usr/include/utmp.h
login: /usr/include/signal.h
login: /usr/include/pwd.h
login: /usr/include/stdio.h
login: /usr/include/sys/stat.h
login: /usr/include/lastlog.h
ls: ls.c
ls: /usr/include/sys/param.h
ls: /usr/include/sys/stat.h
ls: /usr/include/dir.h
ls: /usr/include/stdio.h
ls: /usr/include/sgtty.h
ls: /usr/include/pwd.h
ls: /usr/include/grp.h
ls: /usr/include/utmp.h
mail: mail.c
mail: /usr/include/ctype.h
mail: /usr/include/stdio.h
mail: /usr/include/pwd.h
mail: /usr/include/utmp.h
mail: /usr/include/signal.h
mail: /usr/include/sys/types.h
mail: /usr/include/sys/stat.h
mail: /usr/include/setjmp.h
mail: /usr/include/sysexits.h
mail: /usr/include/sys/socket.h
mail: /usr/include/netinet/in.h
mkdir: mkdir.c
mkdir: /usr/include/signal.h
mkdir: /usr/include/stdio.h
mkdir: /usr/include/sys/types.h
mkdir: /usr/include/stat.h
mt: mt.c
mt: /usr/include/stdio.h
mt: /usr/include/ctype.h
mt: /usr/include/sys/types.h
mt: /usr/include/sys/mtio.h
mt: /usr/include/sys/ioctl.h
mt: /usr/include/vaxmba/mtreg.h
mt: /usr/include/vaxmba/htreg.h
mt: /usr/include/vaxuba/utreg.h
mt: /usr/include/vaxuba/tmreg.h
mt: /usr/include/vaxuba/tsreg.h
mv: mv.c
mv: /usr/include/stdio.h
mv: /usr/include/sys/types.h
mv: /usr/include/sys/stat.h
mv: /usr/include/signal.h
nice: nice.c
nice: /usr/include/stdio.h
nm: nm.c
nm: /usr/include/sys/types.h
nm: /usr/include/ar.h
nm: /usr/include/stdio.h
nm: /usr/include/ctype.h
nm: /usr/include/a.out.h
nm: /usr/include/stab.h
nm: /usr/include/pagsiz.h
nm: /usr/include/stat.h
od: od.c
od: /usr/include/stdio.h
passwd: passwd.c
passwd: /usr/include/stdio.h
passwd: /usr/include/signal.h
passwd: /usr/include/pwd.h
pr: pr.c
pr: /usr/include/stdio.h
pr: /usr/include/signal.h
pr: /usr/include/sys/types.h
pr: /usr/include/sys/stat.h
pwd: pwd.c
pwd: /usr/include/stdio.h
pwd: /usr/include/sys/param.h
pwd: /usr/include/sys/stat.h
pwd: /usr/include/dir.h
rm: rm.c
rm: /usr/include/stdio.h
rm: /usr/include/sys/param.h
rm: /usr/include/sys/stat.h
rm: /usr/include/dir.h
rmail: rmail.c
rmail: /usr/include/stdio.h
rmdir: rmdir.c
rmdir: /usr/include/sys/param.h
rmdir: /usr/include/sys/stat.h
rmdir: /usr/include/dir.h
rmdir: /usr/include/stdio.h
size: size.c
size: /usr/include/stdio.h
size: /usr/include/a.out.h
strip: strip.c
strip: /usr/include/a.out.h
strip: /usr/include/signal.h
strip: /usr/include/pagsiz.h
stty: stty.c
stty: /usr/include/stdio.h
stty: /usr/include/sgtty.h
su: su.c
su: /usr/include/stdio.h
su: /usr/include/pwd.h
sync: sync.c
tar: tar.c
tar: /usr/include/stdio.h
tar: /usr/include/sys/param.h
tar: /usr/include/sys/stat.h
tar: /usr/include/dir.h
tar: /usr/include/sys/ioctl.h
tar: /usr/include/sys/mtio.h
tar: /usr/include/signal.h
tee: tee.c
tee: /usr/include/signal.h
tee: /usr/include/sys/types.h
tee: /usr/include/sys/stat.h
tee: /usr/include/errno.h
tee: /usr/include/pagsiz.h
test: test.c
test: /usr/include/stdio.h
test: /usr/include/sys/types.h
test: /usr/include/sys/stat.h
time: time.c
time: /usr/include/stdio.h
time: /usr/include/signal.h
time: /usr/include/sys/types.h
time: /usr/include/sys/times.h
wall: wall.c
wall: /usr/include/stdio.h
wall: /usr/include/utmp.h
wall: /usr/include/time.h
wall: /usr/include/signal.h
who: who.c
who: /usr/include/stdio.h
who: /usr/include/utmp.h
who: /usr/include/pwd.h
who: /usr/include/ctype.h
write: write.c
write: /usr/include/stdio.h
write: /usr/include/sys/types.h
write: /usr/include/sys/stat.h
write: /usr/include/signal.h
write: /usr/include/utmp.h
write: /usr/include/time.h
ps: ps.c
ps: /usr/include/stdio.h
ps: /usr/include/ctype.h
ps: /usr/include/nlist.h
ps: /usr/include/pwd.h
ps: /usr/include/sys/param.h
ps: /usr/include/sys/tty.h
ps: /usr/include/dir.h
ps: /usr/include/sys/user.h
ps: /usr/include/sys/proc.h
ps: /usr/include/machine/pte.h
ps: /usr/include/sys/vm.h
ps: /usr/include/sys/text.h
ps: /usr/include/sys/stat.h
ps: /usr/include/math.h
ps: /usr/include/sys/vlimit.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
