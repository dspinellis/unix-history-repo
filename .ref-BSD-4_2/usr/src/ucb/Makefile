#	Makefile	4.49	83/09/27
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	Mail compact dbx error ex eyacc fp ftp gprof indent lisp \
	more msgs netstat pascal sendbug talk tftp \
	tset vgrind vpr vlp sysline

# Shell scripts that need only be installed and are never removed.
#
SHSCRIPT= help print trman
CSHSCRIPT= which

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	apply arcv biff checknr chfn chsh colcrt colrm ctags expand \
	finger fold from gcore grep groups head last \
	lastcomm leave lock man mkstr printenv prmail pti quota rcp \
	reset rlogin rsh rwho sccs script soelim strings symorder tail \
	telnet unexpand unifdef users vmstat w whereis whoami \
	whois what wc xstr yes

# Programs that must run setuid to root
#
SETUID=	chfn chsh quota rcp rlogin rsh

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	clear ruptime ul

all:	${SUBDIR} ${STD} ${NSTD}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

${STD}:
	cc ${CFLAGS} -o $@ $@.c

install:
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	for i in ${SHSCRIPT}; do \
		(install -c $$i.sh ${DESTDIR}/usr/ucb/$$i); done
	for i in ${CSHSCRIPT}; do \
		(install -c $$i.csh ${DESTDIR}/usr/ucb/$$i); done
	for i in ${STD} ${NSTD}; do \
		(install -s $$i ${DESTDIR}/usr/ucb/$$i); \
	done
	for i in ${SETUID}; do (chown root ${DESTDIR}/usr/ucb/$$i; \
		chmod 4755 ${DESTDIR}/usr/ucb/$$i); done
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
	rm -f ${STD} ${NSTD}

FRC:

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

clear:
	cc -o clear ${CFLAGS} clear.c -ltermcap

ruptime:
	cc -o ruptime ${CFLAGS} ruptime.c -ltermlib

ul:
	cc -o ul ${CFLAGS} ul.c -ltermcap

# DO NOT DELETE THIS LINE -- make depend uses it

apply: apply.c
apply: /usr/include/stdio.h
arcv: arcv.c
arcv: /usr/include/signal.h
arcv: /usr/include/ar.h
biff: biff.c
biff: /usr/include/sys/types.h
biff: /usr/include/sys/stat.h
biff: /usr/include/stdio.h
checknr: checknr.c
checknr: /usr/include/stdio.h
checknr: /usr/include/ctype.h
chfn: chfn.c
chfn: /usr/include/stdio.h
chfn: /usr/include/signal.h
chfn: /usr/include/pwd.h
chfn: /usr/include/sys/time.h
chfn: /usr/include/sys/resource.h
chfn: /usr/include/sys/file.h
chfn: /usr/include/ctype.h
chsh: chsh.c
chsh: /usr/include/stdio.h
chsh: /usr/include/signal.h
chsh: /usr/include/pwd.h
chsh: /usr/include/sys/file.h
chsh: /usr/include/sys/time.h
chsh: /usr/include/sys/resource.h
colcrt: colcrt.c
colcrt: /usr/include/stdio.h
colrm: colrm.c
colrm: /usr/include/stdio.h
ctags: ctags.c
ctags: /usr/include/stdio.h
ctags: /usr/include/ctype.h
expand: expand.c
expand: /usr/include/stdio.h
finger: finger.c
finger: /usr/include/sys/types.h
finger: /usr/include/sys/stat.h
finger: /usr/include/sgtty.h
finger: /usr/include/utmp.h
finger: /usr/include/signal.h
finger: /usr/include/pwd.h
finger: /usr/include/stdio.h
finger: /usr/include/lastlog.h
finger: /usr/include/sys/time.h
fold: fold.c
fold: /usr/include/stdio.h
from: from.c
from: /usr/include/stdio.h
from: /usr/include/ctype.h
from: /usr/include/pwd.h
gcore: gcore.c
gcore: /usr/include/stdio.h
gcore: /usr/include/nlist.h
gcore: /usr/include/sys/param.h
gcore: /usr/include/sys/dir.h
gcore: /usr/include/sys/user.h
gcore: /usr/include/sys/proc.h
gcore: /usr/include/machine/pte.h
gcore: /usr/include/sys/vm.h
gcore: /usr/include/setjmp.h
grep: grep.c
grep: /usr/include/stdio.h
groups: groups.c
groups: /usr/include/sys/param.h
groups: /usr/include/grp.h
groups: /usr/include/pwd.h
head: head.c
head: /usr/include/stdio.h
last: last.c
last: /usr/include/sys/types.h
last: /usr/include/stdio.h
last: /usr/include/signal.h
last: /usr/include/sys/stat.h
last: /usr/include/utmp.h
lastcomm: lastcomm.c
lastcomm: /usr/include/sys/param.h
lastcomm: /usr/include/sys/acct.h
lastcomm: /usr/include/sys/file.h
lastcomm: /usr/include/stdio.h
lastcomm: /usr/include/pwd.h
lastcomm: /usr/include/sys/stat.h
lastcomm: /usr/include/utmp.h
lastcomm: /usr/include/struct.h
lastcomm: /usr/include/ctype.h
lastcomm: /usr/include/sys/dir.h
leave: leave.c
leave: /usr/include/stdio.h
leave: /usr/include/signal.h
leave: /usr/include/utmp.h
lock: lock.c
lock: /usr/include/stdio.h
lock: /usr/include/sys/types.h
lock: /usr/include/sys/stat.h
lock: /usr/include/signal.h
lock: /usr/include/sgtty.h
man: man.c
man: /usr/include/stdio.h
man: /usr/include/ctype.h
man: /usr/include/sgtty.h
man: /usr/include/sys/types.h
man: /usr/include/sys/stat.h
man: /usr/include/signal.h
mkstr: mkstr.c
mkstr: /usr/include/stdio.h
mkstr: /usr/include/sys/types.h
mkstr: /usr/include/sys/stat.h
printenv: printenv.c
prmail: prmail.c
prmail: /usr/include/pwd.h
prmail: /usr/include/sys/types.h
prmail: /usr/include/sys/stat.h
pti: pti.c
quota: quota.c
quota: /usr/include/stdio.h
quota: /usr/include/fstab.h
quota: /usr/include/ctype.h
quota: /usr/include/pwd.h
quota: /usr/include/sys/param.h
quota: /usr/include/sys/quota.h
quota: /usr/include/sys/file.h
quota: /usr/include/sys/stat.h
rcp: rcp.c
rcp: /usr/include/sys/param.h
rcp: /usr/include/sys/stat.h
rcp: /usr/include/sys/ioctl.h
rcp: /usr/include/netinet/in.h
rcp: /usr/include/stdio.h
rcp: /usr/include/signal.h
rcp: /usr/include/pwd.h
rcp: /usr/include/ctype.h
rcp: /usr/include/errno.h
rcp: /usr/include/sys/dir.h
reset: reset.c
reset: /usr/include/sgtty.h
rlogin: rlogin.c
rlogin: /usr/include/sys/types.h
rlogin: /usr/include/sys/socket.h
rlogin: /usr/include/sys/wait.h
rlogin: /usr/include/netinet/in.h
rlogin: /usr/include/stdio.h
rlogin: /usr/include/sgtty.h
rlogin: /usr/include/errno.h
rlogin: /usr/include/pwd.h
rlogin: /usr/include/signal.h
rlogin: /usr/include/netdb.h
rsh: rsh.c
rsh: /usr/include/sys/types.h
rsh: /usr/include/sys/socket.h
rsh: /usr/include/sys/ioctl.h
rsh: /usr/include/sys/file.h
rsh: /usr/include/netinet/in.h
rsh: /usr/include/stdio.h
rsh: /usr/include/errno.h
rsh: /usr/include/signal.h
rsh: /usr/include/pwd.h
rsh: /usr/include/netdb.h
rwho: rwho.c
rwho: /usr/include/sys/param.h
rwho: /usr/include/stdio.h
rwho: /usr/include/sys/dir.h
rwho: ../etc/rwhod/rwhod.h
sccs: sccs.c
sccs: /usr/include/stdio.h
sccs: /usr/include/sys/param.h
sccs: /usr/include/sys/stat.h
sccs: /usr/include/sys/dir.h
sccs: /usr/include/errno.h
sccs: /usr/include/signal.h
sccs: /usr/include/sysexits.h
sccs: /usr/include/pwd.h
script: script.c
script: /usr/include/stdio.h
script: /usr/include/signal.h
script: /usr/include/sys/types.h
script: /usr/include/sys/stat.h
script: /usr/include/sys/ioctl.h
script: /usr/include/sgtty.h
script: /usr/include/sys/time.h
script: /usr/include/sys/wait.h
soelim: soelim.c
soelim: /usr/include/stdio.h
strings: strings.c
strings: /usr/include/stdio.h
strings: /usr/include/a.out.h
strings: /usr/include/ctype.h
symorder: symorder.c
symorder: /usr/include/stdio.h
symorder: /usr/include/sys/types.h
symorder: /usr/include/sys/stat.h
symorder: /usr/include/a.out.h
tail: tail.c
tail: /usr/include/stdio.h
tail: /usr/include/ctype.h
tail: /usr/include/sys/types.h
tail: /usr/include/sys/stat.h
tail: /usr/include/errno.h
telnet: telnet.c
telnet: /usr/include/sys/types.h
telnet: /usr/include/sys/socket.h
telnet: /usr/include/sys/ioctl.h
telnet: /usr/include/netinet/in.h
telnet: /usr/include/arpa/telnet.h
telnet: /usr/include/stdio.h
telnet: /usr/include/ctype.h
telnet: /usr/include/errno.h
telnet: /usr/include/signal.h
telnet: /usr/include/setjmp.h
telnet: /usr/include/netdb.h
unexpand: unexpand.c
unexpand: /usr/include/stdio.h
unifdef: unifdef.c
unifdef: /usr/include/stdio.h
unifdef: /usr/include/ctype.h
users: users.c
users: /usr/include/stdio.h
users: /usr/include/utmp.h
vmstat: vmstat.c
vmstat: /usr/include/stdio.h
vmstat: /usr/include/sys/param.h
vmstat: /usr/include/sys/vm.h
vmstat: /usr/include/sys/dk.h
vmstat: /usr/include/nlist.h
vmstat: /usr/include/sys/buf.h
vmstat: /usr/include/vaxuba/ubavar.h
vmstat: /usr/include/vaxmba/mbavar.h
w: w.c
w: /usr/include/sys/param.h
w: /usr/include/nlist.h
w: /usr/include/stdio.h
w: /usr/include/ctype.h
w: /usr/include/utmp.h
w: /usr/include/sys/stat.h
w: /usr/include/sys/dir.h
w: /usr/include/sys/user.h
w: /usr/include/sys/proc.h
w: /usr/include/machine/pte.h
w: /usr/include/sys/vm.h
whatis: whatis.c
whatis: /usr/include/stdio.h
whatis: /usr/include/ctype.h
whereis: whereis.c
whereis: /usr/include/sys/param.h
whereis: /usr/include/stdio.h
whereis: /usr/include/ctype.h
whereis: /usr/include/sys/dir.h
whoami: whoami.c
whoami: /usr/include/pwd.h
whois: whois.c
whois: /usr/include/sys/types.h
whois: /usr/include/sys/socket.h
whois: /usr/include/netinet/in.h
whois: /usr/include/stdio.h
whois: /usr/include/netdb.h
what: what.c
what: /usr/include/stdio.h
wc: wc.c
wc: /usr/include/stdio.h
xstr: xstr.c
xstr: /usr/include/stdio.h
xstr: /usr/include/ctype.h
xstr: /usr/include/sys/types.h
xstr: /usr/include/signal.h
yes: yes.c
clear: clear.c
clear: /usr/include/stdio.h
clear: /usr/include/sgtty.h
ruptime: ruptime.c
ruptime: /usr/include/sys/param.h
ruptime: /usr/include/stdio.h
ruptime: /usr/include/sys/dir.h
ruptime: ../etc/rwhod/rwhod.h
ul: ul.c
ul: /usr/include/stdio.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
