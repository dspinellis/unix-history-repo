#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.22 (Berkeley) 6/3/87
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	adb as awk csh diff make sed sh tp

# Shell scripts that need only be installed and are never removed.
#
SCRIPT=	false true

# C source that lives in the current directory
#
SRCS=	ar.c cat.c cc.c chgrp.c chmod.c cmp.c date.c dd.c du.c echo.c ed.c \
	expr.c grep.c hostid.c hostname.c kill.c ld.c ln.c ls.c machine.c \
	mkdir.c mt.c nice.c nm.c od.c pagesize.c pr.c ps.c pwd.c rm.c \
	rmail.c rmdir.c size.c stty.c sync.c tar.c tee.c test.c time.c who.c

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	ar cat cc chgrp chmod cmp date dd du echo ed grep hostid \
	hostname kill ld ln ls machine mkdir mt nice nm od pagesize pr \
	pwd rm rmail rmdir size stty sync tar tee test time who

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	expr ps

# Programs that get installed with special permissions
#
PERM=	df login mail passwd rcp su wall write

BINS=	${STD} ${NSTD} ${PERM} cp mv strip

all:	${SUBDIR} ${BINS}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

${STD} ${PERM} mv cp strip:
	${CC} ${CFLAGS} -o $@ $@.c

# take care with cp, strip, and mv, install uses them
install: FRC
	install -s -o bin -g bin -m 755 cp ${DESTDIR}/bin/newcp; \
		mv ${DESTDIR}/bin/newcp ${DESTDIR}/bin/cp;
	install -s -o bin -g bin -m 755 strip ${DESTDIR}/bin/newstrip; \
		mv ${DESTDIR}/bin/newstrip ${DESTDIR}/bin/strip;
	install -c -s -o bin -g bin -m 755 mv ${DESTDIR}/bin/mv
	rm -f mv
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${SCRIPT}; do \
		(install -c -o bin -g bin -m 755 $$i.sh ${DESTDIR}/bin/$$i); \
	done
	-for i in ${STD}; do \
		(install -s -o bin -g bin -m 755 $$i ${DESTDIR}/bin/$$i); \
	done
# programs that run with special permissions
	install -s -o root -o bin -m 4755 login ${DESTDIR}/bin/login
	install -s -o root -o bin -m 4755 mail ${DESTDIR}/bin/mail
	install -s -o root -o bin -m 4755 passwd ${DESTDIR}/bin/passwd
	install -s -o root -o bin -m 4755 rcp ${DESTDIR}/bin/rcp
	install -s -o root -o bin -m 4755 su ${DESTDIR}/bin/su
	install -s -o bin -g operator -m 2755 df ${DESTDIR}/bin/df
	install -s -o bin -g kmem -m 2755 ps ${DESTDIR}/bin/ps
	install -s -o bin -g tty -m 2755 wall ${DESTDIR}/bin/wall
	install -s -o bin -g tty -m 2755 write ${DESTDIR}/bin/write
	install -s -o bin -g bin -m 755 expr ${DESTDIR}/bin/expr
# directories and links
	rm -f ${DESTDIR}/bin/[; ln ${DESTDIR}/bin/test ${DESTDIR}/bin/[
	rm -f ${DESTDIR}/bin/e; ln ${DESTDIR}/bin/ed ${DESTDIR}/bin/e
	rm -f ${DESTDIR}/bin/chfn; ln ${DESTDIR}/bin/passwd ${DESTDIR}/bin/chfn
	rm -f ${DESTDIR}/bin/chsh; ln ${DESTDIR}/bin/passwd ${DESTDIR}/bin/chsh

clean: FRC
	rm -f ${BINS} expr.c a.out core *.s *.o errs
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done

depend: expr.c FRC
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done
	mkdep ${CFLAGS} ${SRCS}

FRC:

# Files listed in ${NSTD} have explicit make lines given below.

expr: expr.o
	${CC} ${CFLAGS} -o $@ expr.o

ps:
	${CC} ${CFLAGS} -o $@ ps.c -lm

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

ar.o: ar.c /usr/include/sys/param.h /usr/include/sys/types.h
ar.o: /usr/include/signal.h /usr/include/machine/machparam.h
ar.o: /usr/include/sys/stat.h /usr/include/sys/time.h /usr/include/time.h
ar.o: /usr/include/stdio.h /usr/include/ar.h
cat.o: cat.c /usr/include/stdio.h /usr/include/sys/types.h
cat.o: /usr/include/sys/stat.h
cc.o: cc.c /usr/include/sys/param.h /usr/include/sys/types.h
cc.o: /usr/include/signal.h /usr/include/machine/machparam.h
cc.o: /usr/include/stdio.h /usr/include/ctype.h /usr/include/signal.h
cc.o: /usr/include/sys/dir.h
chgrp.o: chgrp.c /usr/include/stdio.h /usr/include/ctype.h
chgrp.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/grp.h
chgrp.o: /usr/include/pwd.h /usr/include/sys/dir.h
chmod.o: chmod.c /usr/include/stdio.h /usr/include/sys/types.h
chmod.o: /usr/include/sys/stat.h /usr/include/sys/dir.h
cmp.o: cmp.c /usr/include/sys/param.h /usr/include/sys/types.h
cmp.o: /usr/include/signal.h /usr/include/machine/machparam.h
cmp.o: /usr/include/sys/file.h /usr/include/sys/stat.h /usr/include/stdio.h
cmp.o: /usr/include/ctype.h
date.o: date.c /usr/include/sys/param.h /usr/include/sys/types.h
date.o: /usr/include/signal.h /usr/include/machine/machparam.h
date.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/file.h
date.o: /usr/include/errno.h /usr/include/syslog.h /usr/include/utmp.h
date.o: /usr/include/tzfile.h /usr/include/stdio.h /usr/include/ctype.h
date.o: /usr/include/strings.h /usr/include/sys/socket.h
date.o: /usr/include/netinet/in.h /usr/include/netdb.h
date.o: /usr/include/protocols/timed.h
dd.o: dd.c /usr/include/sys/file.h /usr/include/stdio.h /usr/include/signal.h
du.o: du.c /usr/include/stdio.h /usr/include/sys/param.h
du.o: /usr/include/sys/types.h /usr/include/signal.h
du.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
du.o: /usr/include/sys/dir.h
echo.o: echo.c /usr/include/stdio.h
ed.o: ed.c /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ed.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ed.o: /usr/include/setjmp.h
grep.o: grep.c /usr/include/stdio.h /usr/include/ctype.h
hostid.o: hostid.c /usr/include/sys/types.h /usr/include/stdio.h
hostid.o: /usr/include/ctype.h /usr/include/netdb.h
hostname.o: hostname.c /usr/include/stdio.h
kill.o: kill.c /usr/include/signal.h /usr/include/ctype.h
ld.o: ld.c /usr/include/sys/param.h /usr/include/sys/types.h
ld.o: /usr/include/signal.h /usr/include/machine/machparam.h
ld.o: /usr/include/signal.h /usr/include/stdio.h /usr/include/ctype.h
ld.o: /usr/include/ar.h /usr/include/a.out.h /usr/include/sys/exec.h
ld.o: /usr/include/ranlib.h /usr/include/sys/stat.h /usr/include/sys/file.h
ln.o: ln.c /usr/include/stdio.h /usr/include/sys/types.h
ln.o: /usr/include/sys/stat.h /usr/include/errno.h
ls.o: ls.c /usr/include/sys/param.h /usr/include/sys/types.h
ls.o: /usr/include/signal.h /usr/include/machine/machparam.h
ls.o: /usr/include/sys/stat.h /usr/include/sys/dir.h /usr/include/stdio.h
ls.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ls.o: /usr/include/sys/ttydev.h /usr/include/pwd.h /usr/include/grp.h
ls.o: /usr/include/utmp.h
machine.o: machine.c /usr/include/sys/param.h /usr/include/sys/types.h
machine.o: /usr/include/signal.h /usr/include/machine/machparam.h
mkdir.o: mkdir.c /usr/include/stdio.h
mt.o: mt.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/sys/types.h
mt.o: /usr/include/sys/mtio.h /usr/include/sys/ioctl.h
mt.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
mt.o: /usr/include/tahoevba/cyreg.h
nice.o: nice.c /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
nice.o: /usr/include/sys/resource.h
nm.o: nm.c /usr/include/sys/types.h /usr/include/sys/file.h /usr/include/ar.h
nm.o: /usr/include/stdio.h /usr/include/ctype.h /usr/include/a.out.h
nm.o: /usr/include/sys/exec.h /usr/include/stab.h /usr/include/ranlib.h
od.o: od.c /usr/include/stdio.h /usr/include/sys/types.h
od.o: /usr/include/sys/stat.h
pagesize.o: pagesize.c
pr.o: pr.c /usr/include/stdio.h /usr/include/signal.h /usr/include/sys/types.h
pr.o: /usr/include/sys/stat.h
pwd.o: pwd.c /usr/include/stdio.h /usr/include/sys/param.h
pwd.o: /usr/include/sys/types.h /usr/include/signal.h
pwd.o: /usr/include/machine/machparam.h
rm.o: rm.c /usr/include/stdio.h /usr/include/sys/param.h
rm.o: /usr/include/sys/types.h /usr/include/signal.h
rm.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
rm.o: /usr/include/sys/dir.h /usr/include/sys/file.h
rmail.o: rmail.c /usr/include/stdio.h /usr/include/sysexits.h
rmdir.o: rmdir.c /usr/include/stdio.h
size.o: size.c /usr/include/stdio.h /usr/include/a.out.h
size.o: /usr/include/sys/exec.h
stty.o: stty.c /usr/include/stdio.h /usr/include/sys/ioctl.h
stty.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
sync.o: sync.c
tar.o: tar.c /usr/include/stdio.h /usr/include/sys/param.h
tar.o: /usr/include/sys/types.h /usr/include/signal.h
tar.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
tar.o: /usr/include/sys/dir.h /usr/include/sys/ioctl.h
tar.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tar.o: /usr/include/sys/mtio.h /usr/include/sys/time.h /usr/include/time.h
tar.o: /usr/include/signal.h /usr/include/errno.h /usr/include/fcntl.h
tee.o: tee.c /usr/include/signal.h /usr/include/sys/types.h
tee.o: /usr/include/sys/stat.h /usr/include/errno.h
test.o: test.c /usr/include/stdio.h /usr/include/sys/types.h
test.o: /usr/include/sys/stat.h
time.o: time.c /usr/include/stdio.h /usr/include/signal.h
time.o: /usr/include/sys/types.h /usr/include/sys/time.h /usr/include/time.h
time.o: /usr/include/sys/resource.h
who.o: who.c /usr/include/stdio.h /usr/include/utmp.h /usr/include/pwd.h
who.o: /usr/include/ctype.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
