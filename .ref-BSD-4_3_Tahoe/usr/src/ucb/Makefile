#
# Copyright (c) 1980, 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.26 (Berkeley) 6/29/88
#
DESTDIR=
CFLAGS=	-O
LIBC=	/lib/libc.a

# Programs that live in subdirectories, and have makefiles of their own.
#
# fp removed; it doesn't work
SUBDIR=	Mail compress ctags dbx error ex ftp gprof grep indent lisp man \
	more msgs netstat pascal rdist sendbug systat talk telnet tftp \
	tn3270 tset vgrind vlp sysline window

# Shell scripts that need only be installed and are never removed.
#
SCRIPT= which

# C source that lives in the current directory
#
SRCS=	apply.c biff.c checknr.c clear.c colcrt.c colrm.c expand.c finger.c \
	fold.c from.c gcore.c groups.c head.c last.c lastcomm.c \
	leave.c lock.c logger.c mkstr.c printenv.c quota.c rlogin.c \
	rsh.c ruptime.c rwho.c sccs.c script.c soelim.c strings.c symorder.c \
	tail.c tcopy.c ul.c unexpand.c unifdef.c users.c \
	vacation.c vmstat.c w.c wc.c what.c whereis.c whoami.c whois.c \
	xstr.c yes.c

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	apply biff checknr colcrt colrm expand finger fold from \
	groups head last lastcomm leave lock logger mkstr printenv \
	ruptime rwho sccs script soelim strings symorder tail tcopy \
	unexpand unifdef users whereis whoami whois what wc xstr yes

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	clear ul vacation

# Programs that get installed with special permissions
#
PERM=	gcore quota rlogin rsh vmstat w

all:	${SUBDIR} ${STD} ${NSTD} ${PERM}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

${STD} ${PERM}: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${SCRIPT}; do \
		(install -c -o bin -g bin -m 755 $$i.csh ${DESTDIR}/usr/ucb/$$i); done
	install -s -o bin -g bin -m 755 ${STD} ${NSTD} ${DESTDIR}/usr/ucb
	install -s -o root -g bin -m 4755 quota ${DESTDIR}/usr/ucb/quota
	install -s -o root -g bin -m 4755 rlogin ${DESTDIR}/usr/ucb/rlogin
	install -s -o root -g bin -m 4755 rsh ${DESTDIR}/usr/ucb/rsh
	install -s -o bin -g kmem -m 2755 gcore ${DESTDIR}/usr/ucb/gcore
	install -s -o bin -g kmem -m 2755 vmstat ${DESTDIR}/usr/ucb/vmstat
	install -s -o bin -g kmem -m 2755 w ${DESTDIR}/usr/ucb/w
	rm -f ${DESTDIR}/usr/ucb/uptime ${DESTDIR}/usr/ucb/f
	ln ${DESTDIR}/usr/ucb/w ${DESTDIR}/usr/ucb/uptime
	ln ${DESTDIR}/usr/ucb/finger ${DESTDIR}/usr/ucb/f
	rm -f ${DESTDIR}/usr/ucb/u
	ln ${DESTDIR}/usr/ucb/users ${DESTDIR}/usr/ucb/u

clean: FRC
	rm -f a.out core *.s *.o
	rm -f ${STD} ${NSTD} ${PERM}
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done

depend: ldepend
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done

ldepend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

FRC:

# Files listed in ${NSTD} have explicit make lines given below.

clear:
	${CC} -o $@ ${CFLAGS} clear.c -ltermlib

ul:
	${CC} -o $@ ${CFLAGS} ul.c -ltermlib

vacation:
	${CC} -o $@ ${CFLAGS} vacation.c -ldbm

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

apply: apply.c /usr/include/stdio.h
biff: biff.c /usr/include/sys/types.h /usr/include/sys/stat.h
biff: /usr/include/stdio.h
checknr: checknr.c /usr/include/stdio.h /usr/include/ctype.h
clear: clear.c /usr/include/stdio.h /usr/include/sgtty.h
clear: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
clear: /usr/include/sys/ttydev.h
colcrt: colcrt.c /usr/include/stdio.h
colrm: colrm.c /usr/include/stdio.h
expand: expand.c /usr/include/stdio.h
finger: finger.c /usr/include/sys/types.h /usr/include/sys/stat.h
finger: /usr/include/utmp.h /usr/include/sys/signal.h
finger: /usr/include/machine/trap.h /usr/include/pwd.h /usr/include/stdio.h
finger: /usr/include/lastlog.h /usr/include/ctype.h /usr/include/sys/time.h
finger: /usr/include/time.h /usr/include/sys/socket.h /usr/include/netinet/in.h
finger: /usr/include/netdb.h
fold: fold.c /usr/include/stdio.h
from: from.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/pwd.h
gcore: gcore.c /usr/include/stdio.h /usr/include/nlist.h
gcore: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
gcore: /usr/include/machine/trap.h /usr/include/machine/machparam.h
gcore: /usr/include/machine/endian.h /usr/include/sys/dir.h
gcore: /usr/include/sys/user.h /usr/include/machine/pcb.h
gcore: /usr/include/sys/dmap.h /usr/include/sys/time.h /usr/include/time.h
gcore: /usr/include/sys/resource.h /usr/include/sys/namei.h
gcore: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/sys/proc.h
gcore: /usr/include/machine/pte.h /usr/include/sys/vm.h
gcore: /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
gcore: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
gcore: /usr/include/sys/vmsystm.h /usr/include/setjmp.h
groups: groups.c /usr/include/sys/param.h /usr/include/sys/types.h
groups: /usr/include/signal.h /usr/include/machine/trap.h
groups: /usr/include/machine/machparam.h /usr/include/machine/endian.h
groups: /usr/include/grp.h /usr/include/pwd.h /usr/include/stdio.h
head: head.c /usr/include/stdio.h /usr/include/ctype.h
last: last.c /usr/include/sys/param.h /usr/include/sys/types.h
last: /usr/include/signal.h /usr/include/machine/trap.h
last: /usr/include/machine/machparam.h /usr/include/machine/endian.h
last: /usr/include/sys/stat.h /usr/include/sys/file.h /usr/include/signal.h
last: /usr/include/time.h /usr/include/utmp.h /usr/include/stdio.h
lastcomm: lastcomm.c /usr/include/sys/param.h /usr/include/sys/types.h
lastcomm: /usr/include/signal.h /usr/include/machine/trap.h
lastcomm: /usr/include/machine/machparam.h /usr/include/machine/endian.h
lastcomm: /usr/include/sys/acct.h /usr/include/sys/file.h
lastcomm: /usr/include/sys/stat.h /usr/include/utmp.h /usr/include/struct.h
lastcomm: /usr/include/ctype.h /usr/include/stdio.h /usr/include/pwd.h
lastcomm: /usr/include/sys/dir.h
leave: leave.c /usr/include/sys/param.h /usr/include/sys/types.h
leave: /usr/include/signal.h /usr/include/machine/trap.h
leave: /usr/include/machine/machparam.h /usr/include/machine/endian.h
leave: /usr/include/sys/time.h /usr/include/time.h /usr/include/stdio.h
leave: /usr/include/ctype.h
lock: lock.c /usr/include/sys/param.h /usr/include/sys/types.h
lock: /usr/include/signal.h /usr/include/machine/trap.h
lock: /usr/include/machine/machparam.h /usr/include/machine/endian.h
lock: /usr/include/sys/stat.h /usr/include/sys/time.h /usr/include/time.h
lock: /usr/include/sys/signal.h /usr/include/pwd.h /usr/include/sgtty.h
lock: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
lock: /usr/include/sys/ttydev.h /usr/include/stdio.h /usr/include/ctype.h
logger: logger.c /usr/include/stdio.h /usr/include/syslog.h
logger: /usr/include/ctype.h
mkstr: mkstr.c /usr/include/stdio.h /usr/include/sys/types.h
mkstr: /usr/include/sys/stat.h
printenv: printenv.c
quota: quota.c /usr/include/stdio.h /usr/include/fstab.h /usr/include/ctype.h
quota: /usr/include/pwd.h /usr/include/errno.h /usr/include/sys/param.h
quota: /usr/include/sys/types.h /usr/include/signal.h
quota: /usr/include/machine/trap.h /usr/include/machine/machparam.h
quota: /usr/include/machine/endian.h /usr/include/sys/quota.h
quota: /usr/include/sys/file.h /usr/include/sys/stat.h
rlogin: rlogin.c /usr/include/sys/param.h /usr/include/sys/types.h
rlogin: /usr/include/signal.h /usr/include/machine/trap.h
rlogin: /usr/include/machine/machparam.h /usr/include/machine/endian.h
rlogin: /usr/include/sys/errno.h /usr/include/sys/file.h
rlogin: /usr/include/sys/socket.h /usr/include/sys/time.h /usr/include/time.h
rlogin: /usr/include/sys/resource.h /usr/include/sys/wait.h
rlogin: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/sgtty.h
rlogin: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
rlogin: /usr/include/sys/ttydev.h /usr/include/errno.h /usr/include/pwd.h
rlogin: /usr/include/signal.h /usr/include/setjmp.h /usr/include/netdb.h
rsh: rsh.c /usr/include/sys/types.h /usr/include/sys/socket.h
rsh: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
rsh: /usr/include/sys/ttydev.h /usr/include/sys/file.h
rsh: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/errno.h
rsh: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/pwd.h
rsh: /usr/include/netdb.h
ruptime: ruptime.c /usr/include/sys/param.h /usr/include/sys/types.h
ruptime: /usr/include/signal.h /usr/include/machine/trap.h
ruptime: /usr/include/machine/machparam.h /usr/include/machine/endian.h
ruptime: /usr/include/stdio.h /usr/include/sys/dir.h
ruptime: /usr/include/protocols/rwhod.h
rwho: rwho.c /usr/include/sys/param.h /usr/include/sys/types.h
rwho: /usr/include/signal.h /usr/include/machine/trap.h
rwho: /usr/include/machine/machparam.h /usr/include/machine/endian.h
rwho: /usr/include/stdio.h /usr/include/sys/dir.h
rwho: /usr/include/protocols/rwhod.h
sccs: sccs.c /usr/include/stdio.h /usr/include/sys/param.h
sccs: /usr/include/sys/types.h /usr/include/signal.h
sccs: /usr/include/machine/trap.h /usr/include/machine/machparam.h
sccs: /usr/include/machine/endian.h /usr/include/sys/stat.h
sccs: /usr/include/sys/dir.h /usr/include/errno.h /usr/include/signal.h
sccs: /usr/include/sysexits.h /usr/include/pwd.h
script: script.c /usr/include/sys/types.h /usr/include/sys/stat.h
script: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
script: /usr/include/sys/ttydev.h /usr/include/sys/time.h /usr/include/time.h
script: /usr/include/sys/file.h /usr/include/stdio.h /usr/include/signal.h
script: /usr/include/machine/trap.h /usr/include/sys/wait.h
script: /usr/include/machine/endian.h
soelim: soelim.c /usr/include/stdio.h
strings: strings.c /usr/include/sys/types.h /usr/include/sys/file.h
strings: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/stdio.h
strings: /usr/include/ctype.h
symorder: symorder.c /usr/include/stdio.h /usr/include/sys/types.h
symorder: /usr/include/sys/stat.h /usr/include/a.out.h /usr/include/sys/exec.h
tail: tail.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/sys/types.h
tail: /usr/include/sys/stat.h /usr/include/sys/file.h /usr/include/errno.h
tcopy: tcopy.c /usr/include/stdio.h /usr/include/signal.h
tcopy: /usr/include/machine/trap.h /usr/include/sys/file.h
tcopy: /usr/include/sys/types.h /usr/include/sys/ioctl.h
tcopy: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tcopy: /usr/include/sys/mtio.h /usr/include/sys/errno.h
ul: ul.c /usr/include/stdio.h
unexpand: unexpand.c /usr/include/stdio.h
unifdef: unifdef.c /usr/include/stdio.h /usr/include/ctype.h
users: users.c /usr/include/sys/types.h /usr/include/utmp.h
users: /usr/include/stdio.h
vacation: vacation.c /usr/include/sys/param.h /usr/include/sys/types.h
vacation: /usr/include/signal.h /usr/include/machine/trap.h
vacation: /usr/include/machine/machparam.h /usr/include/machine/endian.h
vacation: /usr/include/sys/file.h /usr/include/pwd.h /usr/include/stdio.h
vacation: /usr/include/ctype.h /usr/include/syslog.h
vmstat: vmstat.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/nlist.h
vmstat: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
vmstat: /usr/include/machine/trap.h /usr/include/machine/machparam.h
vmstat: /usr/include/machine/endian.h /usr/include/sys/file.h
vmstat: /usr/include/sys/vm.h /usr/include/sys/vmparam.h
vmstat: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
vmstat: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h
vmstat: /usr/include/sys/dkstat.h /usr/include/sys/buf.h /usr/include/sys/dir.h
vmstat: /usr/include/sys/inode.h /usr/include/sys/namei.h
vmstat: /usr/include/sys/uio.h /usr/include/sys/text.h
vmstat: /usr/include/sys/malloc.h /usr/include/tahoe/cpu.h
vmstat: /usr/include/tahoevba/vbavar.h
w: w.c /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
w: /usr/include/machine/trap.h /usr/include/machine/machparam.h
w: /usr/include/machine/endian.h /usr/include/nlist.h /usr/include/stdio.h
w: /usr/include/ctype.h /usr/include/utmp.h /usr/include/sys/stat.h
w: /usr/include/sys/dir.h /usr/include/sys/user.h /usr/include/machine/pcb.h
w: /usr/include/sys/dmap.h /usr/include/sys/time.h /usr/include/time.h
w: /usr/include/sys/resource.h /usr/include/sys/namei.h /usr/include/sys/uio.h
w: /usr/include/errno.h /usr/include/sys/proc.h /usr/include/sys/ioctl.h
w: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
w: /usr/include/machine/pte.h /usr/include/sys/vm.h /usr/include/sys/vmparam.h
w: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
w: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h /usr/include/sys/tty.h
w: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wc: wc.c /usr/include/sys/param.h /usr/include/sys/types.h
wc: /usr/include/signal.h /usr/include/machine/trap.h
wc: /usr/include/machine/machparam.h /usr/include/machine/endian.h
wc: /usr/include/sys/stat.h /usr/include/sys/file.h /usr/include/stdio.h
what: what.c /usr/include/stdio.h
whereis: whereis.c /usr/include/sys/param.h /usr/include/sys/types.h
whereis: /usr/include/signal.h /usr/include/machine/trap.h
whereis: /usr/include/machine/machparam.h /usr/include/machine/endian.h
whereis: /usr/include/sys/dir.h /usr/include/stdio.h /usr/include/ctype.h
whoami: whoami.c /usr/include/sys/types.h /usr/include/pwd.h
whois: whois.c /usr/include/sys/types.h /usr/include/sys/socket.h
whois: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/netdb.h
xstr: xstr.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/sys/types.h
xstr: /usr/include/signal.h /usr/include/machine/trap.h
yes: yes.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
