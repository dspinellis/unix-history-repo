#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.17 (Berkeley) 6/7/86
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	Mail compress dbx error ex fp ftp gprof indent lisp \
	more msgs netstat pascal rdist sendbug systat talk tftp \
	tn3270 tset vgrind vlp sysline window

# Shell scripts that need only be installed and are never removed.
#
CSHSCRIPT= which

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	apply biff checknr colcrt colrm ctags expand finger fold \
	from grep groups head last lastcomm leave lock logger man mkstr \
	printenv ruptime rwho sccs script soelim strings symorder tail tcopy \
	telnet unexpand unifdef users whereis whoami whois what wc xstr yes

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	clear ul vacation

# Programs that must run setuid to root
#
SETUID=	quota rlogin rsh

# Programs that must run set-group-id kmem.
#
KMEM=	gcore vmstat w

all:	${SUBDIR} ${STD} ${NSTD} ${KMEM} ${SETUID}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

${STD} ${KMEM} ${SETUID}:
	cc ${CFLAGS} -o $@ $@.c

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${CSHSCRIPT}; do \
		(install -m 755 -c $$i.csh ${DESTDIR}/usr/ucb/$$i); done
	-for i in ${STD} ${NSTD}; do \
		(install -s $$i ${DESTDIR}/usr/ucb/$$i); done
	-for i in ${KMEM}; do \
		(install -g kmem -m 2755 -s $$i ${DESTDIR}/usr/ucb/$$i); done
	-for i in ${SETUID}; do \
		(install -o root -m 4755 -s $$i ${DESTDIR}/usr/ucb/$$i); done
	rm -f ${DESTDIR}/usr/ucb/uptime ${DESTDIR}/usr/ucb/f
	ln ${DESTDIR}/usr/ucb/w ${DESTDIR}/usr/ucb/uptime
	ln ${DESTDIR}/usr/ucb/finger ${DESTDIR}/usr/ucb/f
	rm -f ${DESTDIR}/usr/ucb/apropos ${DESTDIR}/usr/ucb/whatis
	ln ${DESTDIR}/usr/ucb/man ${DESTDIR}/usr/ucb/apropos
	ln ${DESTDIR}/usr/ucb/man ${DESTDIR}/usr/ucb/whatis
	rm -f ${DESTDIR}/usr/ucb/u
	ln ${DESTDIR}/usr/ucb/users ${DESTDIR}/usr/ucb/u

clean:
	rm -f a.out core *.s *.o
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done
	rm -f ${STD} ${NSTD} ${SETUID} ${KMEM}

FRC:

depend:
	for i in ${STD} ${NSTD} ${SETUID} ${KMEM}; do \
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

clear:
	${CC} -o clear ${CFLAGS} clear.c -ltermlib

ul:
	${CC} -o ul ${CFLAGS} ul.c -ltermlib

vacation: vacation.c
	${CC} -o vacation ${CFLAGS} vacation.c -ldbm

# DO NOT DELETE THIS LINE -- make depend uses it

apply: apply.c /usr/include/stdio.h
biff: biff.c /usr/include/sys/types.h /usr/include/sys/stat.h
biff: /usr/include/stdio.h
checknr: checknr.c /usr/include/stdio.h /usr/include/ctype.h
colcrt: colcrt.c /usr/include/stdio.h
colrm: colrm.c /usr/include/stdio.h
ctags: ctags.c /usr/include/stdio.h /usr/include/ctype.h
expand: expand.c /usr/include/stdio.h
finger: finger.c /usr/include/sys/types.h /usr/include/sys/stat.h
finger: /usr/include/utmp.h /usr/include/sys/signal.h /usr/include/pwd.h
finger: /usr/include/stdio.h /usr/include/lastlog.h /usr/include/ctype.h
finger: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/socket.h
finger: /usr/include/netinet/in.h /usr/include/netdb.h
fold: fold.c /usr/include/stdio.h
from: from.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/pwd.h
grep: grep.c /usr/include/stdio.h /usr/include/sys/types.h
grep: /usr/include/sys/stat.h
groups: groups.c /usr/include/sys/param.h /usr/include/machine/machparam.h
groups: /usr/include/signal.h /usr/include/sys/types.h /usr/include/grp.h
groups: /usr/include/pwd.h /usr/include/stdio.h
head: head.c /usr/include/stdio.h
last: last.c /usr/include/sys/types.h /usr/include/stdio.h
last: /usr/include/signal.h /usr/include/sys/stat.h /usr/include/utmp.h
lastcomm: lastcomm.c /usr/include/sys/param.h /usr/include/machine/machparam.h
lastcomm: /usr/include/signal.h /usr/include/sys/types.h
lastcomm: /usr/include/sys/acct.h /usr/include/sys/file.h /usr/include/stdio.h
lastcomm: /usr/include/pwd.h /usr/include/sys/stat.h /usr/include/utmp.h
lastcomm: /usr/include/struct.h /usr/include/ctype.h /usr/include/sys/dir.h
leave: leave.c /usr/include/stdio.h /usr/include/signal.h
lock: lock.c /usr/include/pwd.h /usr/include/stdio.h /usr/include/sys/types.h
lock: /usr/include/sys/stat.h /usr/include/sys/time.h /usr/include/time.h
lock: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
lock: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
logger: logger.c /usr/include/stdio.h /usr/include/syslog.h
logger: /usr/include/ctype.h
man: man.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/sgtty.h
man: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
man: /usr/include/sys/ttydev.h /usr/include/sys/param.h
man: /usr/include/machine/machparam.h /usr/include/signal.h
man: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/signal.h
man: /usr/include/strings.h
mkstr: mkstr.c /usr/include/stdio.h /usr/include/sys/types.h
mkstr: /usr/include/sys/stat.h
printenv: printenv.c
ruptime: ruptime.c /usr/include/sys/param.h /usr/include/machine/machparam.h
ruptime: /usr/include/signal.h /usr/include/sys/types.h /usr/include/stdio.h
ruptime: /usr/include/sys/dir.h /usr/include/protocols/rwhod.h
rwho: rwho.c /usr/include/sys/param.h /usr/include/machine/machparam.h
rwho: /usr/include/signal.h /usr/include/sys/types.h /usr/include/stdio.h
rwho: /usr/include/sys/dir.h /usr/include/protocols/rwhod.h
sccs: sccs.c /usr/include/stdio.h /usr/include/sys/param.h
sccs: /usr/include/machine/machparam.h /usr/include/signal.h
sccs: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/sys/dir.h
sccs: /usr/include/errno.h /usr/include/signal.h /usr/include/sysexits.h
sccs: /usr/include/pwd.h
script: script.c /usr/include/stdio.h /usr/include/signal.h
script: /usr/include/sys/types.h /usr/include/sys/stat.h
script: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
script: /usr/include/sys/ttydev.h /usr/include/sgtty.h /usr/include/sys/time.h
script: /usr/include/time.h /usr/include/sys/file.h /usr/include/sys/wait.h
soelim: soelim.c /usr/include/stdio.h
strings: strings.c /usr/include/stdio.h /usr/include/a.out.h
strings: /usr/include/sys/exec.h /usr/include/ctype.h /usr/include/sys/file.h
symorder: symorder.c /usr/include/stdio.h /usr/include/sys/types.h
symorder: /usr/include/sys/stat.h /usr/include/a.out.h /usr/include/sys/exec.h
tail: tail.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/sys/types.h
tail: /usr/include/sys/stat.h /usr/include/sys/file.h /usr/include/errno.h
tcopy: tcopy.c /usr/include/stdio.h /usr/include/signal.h
tcopy: /usr/include/sys/file.h /usr/include/sys/types.h
tcopy: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
tcopy: /usr/include/sys/ttydev.h /usr/include/sys/mtio.h
telnet: telnet.c /usr/include/sys/types.h /usr/include/sys/socket.h
telnet: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
telnet: /usr/include/sys/ttydev.h /usr/include/sys/time.h /usr/include/time.h
telnet: /usr/include/netinet/in.h /usr/include/arpa/telnet.h
telnet: /usr/include/arpa/inet.h /usr/include/stdio.h /usr/include/ctype.h
telnet: /usr/include/errno.h /usr/include/signal.h /usr/include/setjmp.h
telnet: /usr/include/netdb.h /usr/include/strings.h
unexpand: unexpand.c /usr/include/stdio.h
unifdef: unifdef.c /usr/include/stdio.h /usr/include/ctype.h
users: users.c /usr/include/stdio.h /usr/include/utmp.h
whereis: whereis.c /usr/include/sys/param.h /usr/include/machine/machparam.h
whereis: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/dir.h
whereis: /usr/include/stdio.h /usr/include/ctype.h
whoami: whoami.c /usr/include/pwd.h
whois: whois.c /usr/include/sys/types.h /usr/include/sys/socket.h
whois: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/netdb.h
what: what.c /usr/include/stdio.h
wc: wc.c /usr/include/stdio.h
xstr: xstr.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/sys/types.h
xstr: /usr/include/signal.h
yes: yes.c
clear: clear.c /usr/include/stdio.h /usr/include/sgtty.h
clear: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
clear: /usr/include/sys/ttydev.h
ul: ul.c /usr/include/stdio.h
vacation: vacation.c /usr/include/sys/types.h /usr/include/pwd.h
vacation: /usr/include/stdio.h /usr/include/sysexits.h /usr/include/ctype.h
quota: quota.c /usr/include/stdio.h /usr/include/fstab.h /usr/include/ctype.h
quota: /usr/include/pwd.h /usr/include/errno.h /usr/include/sys/param.h
quota: /usr/include/machine/machparam.h /usr/include/signal.h
quota: /usr/include/sys/types.h /usr/include/sys/quota.h
quota: /usr/include/sys/file.h /usr/include/sys/stat.h
rlogin: rlogin.c /usr/include/sys/param.h /usr/include/machine/machparam.h
rlogin: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/errno.h
rlogin: /usr/include/sys/file.h /usr/include/sys/socket.h
rlogin: /usr/include/sys/wait.h /usr/include/netinet/in.h /usr/include/stdio.h
rlogin: /usr/include/sgtty.h /usr/include/sys/ioctl.h
rlogin: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
rlogin: /usr/include/errno.h /usr/include/pwd.h /usr/include/signal.h
rlogin: /usr/include/setjmp.h /usr/include/netdb.h
rsh: rsh.c /usr/include/sys/types.h /usr/include/sys/socket.h
rsh: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
rsh: /usr/include/sys/ttydev.h /usr/include/sys/file.h
rsh: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/errno.h
rsh: /usr/include/signal.h /usr/include/pwd.h /usr/include/netdb.h
gcore: gcore.c /usr/include/stdio.h /usr/include/nlist.h
gcore: /usr/include/sys/param.h /usr/include/machine/machparam.h
gcore: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/dir.h
gcore: /usr/include/sys/user.h /usr/include/machine/pcb.h
gcore: /usr/include/sys/dmap.h /usr/include/sys/time.h /usr/include/time.h
gcore: /usr/include/sys/resource.h /usr/include/sys/namei.h
gcore: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/sys/proc.h
gcore: /usr/include/machine/pte.h /usr/include/sys/vm.h
gcore: /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
gcore: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
gcore: /usr/include/sys/vmsystm.h /usr/include/setjmp.h
vmstat: vmstat.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/nlist.h
vmstat: /usr/include/sys/param.h /usr/include/machine/machparam.h
vmstat: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/file.h
vmstat: /usr/include/sys/vm.h /usr/include/sys/vmparam.h
vmstat: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
vmstat: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h
vmstat: /usr/include/sys/dk.h /usr/include/sys/buf.h /usr/include/sys/dir.h
vmstat: /usr/include/sys/inode.h /usr/include/sys/namei.h
vmstat: /usr/include/sys/uio.h /usr/include/vaxuba/ubavar.h
vmstat: /usr/include/vaxmba/mbavar.h
w: w.c /usr/include/sys/param.h /usr/include/machine/machparam.h
w: /usr/include/signal.h /usr/include/sys/types.h /usr/include/nlist.h
w: /usr/include/stdio.h /usr/include/ctype.h /usr/include/utmp.h
w: /usr/include/sys/stat.h /usr/include/sys/dir.h /usr/include/sys/user.h
w: /usr/include/machine/pcb.h /usr/include/sys/dmap.h /usr/include/sys/time.h
w: /usr/include/time.h /usr/include/sys/resource.h /usr/include/sys/namei.h
w: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/sys/proc.h
w: /usr/include/machine/pte.h /usr/include/sys/vm.h /usr/include/sys/vmparam.h
w: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
w: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
