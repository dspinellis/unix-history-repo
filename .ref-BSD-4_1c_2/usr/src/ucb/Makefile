#	@(#)Makefile	4.28	(Berkeley)	2/27/83
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	Mail berknet compact dbx error ex eyacc fp gprof indent \
	lisp more msgs netser pascal tset vgrind vpr vlp sysline

# Shell scripts that need only be installed and are never removed.
#
SHSCRIPT= apropos help lpr print see trman
CSHSCRIPT= which

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	apply arcv at biff checknr chfn chsh colcrt colrm ctags expand \
	finger fold from gcore grep groups head last \
	lastcomm leave man mkstr num printenv prmail pti reset \
	sccs soelim ssp strings symorder tail unexpand unifdef \
	users vmstat w whatis whereis whoami xstr what wc yes

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	clear lock script ul

all:	${SUBDIR} ${STD} ${NSTD}

${SUBDIR}: /tmp
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
	for i in ${STD} ${NSTD}; do (install $$i ${DESTDIR}/usr/ucb/$$i); done
	rm -f ${DESTDIR}/usr/ucb/uptime ${DESTDIR}/usr/ucb/page \
		${DESTDIR}/usr/ucb/f
	ln ${DESTDIR}/usr/ucb/w ${DESTDIR}/usr/ucb/uptime
	ln ${DESTDIR}/usr/ucb/finger ${DESTDIR}/usr/ucb/f
	chmod 4755 ${DESTDIR}/usr/ucb/chsh ${DESTDIR}/usr/ucb/chfn

clean:
	rm -f a.out core *.s *.o
	for i in ${SUBDIR}; do (echo $$i; cd $$i; make ${MFLAGS} clean); done
	rm -f ${STD} ${NSTD}

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
	cc -o clear clear.c -ltermcap

lock:
	cc -o lock lock.c -ljobs

script:
	cc -o script script.c -ljobs

ul:
	cc -o ul ul.c -ltermcap

# DO NOT DELETE THIS LINE -- make depend uses it

apply: apply.c
apply: /usr/include/stdio.h
arcv: arcv.c
arcv: /usr/include/signal.h
arcv: /usr/include/ar.h
at: at.c
at: /usr/include/stdio.h
at: /usr/include/ctype.h
at: /usr/include/time.h
at: /usr/include/signal.h
biff: biff.c
biff: /usr/include/sys/types.h
biff: /usr/include/stat.h
biff: /usr/include/stdio.h
checknr: checknr.c
checknr: /usr/include/stdio.h
checknr: /usr/include/ctype.h
chfn: chfn.c
chfn: /usr/include/stdio.h
chfn: /usr/include/signal.h
chfn: /usr/include/pwd.h
chsh: chsh.c
chsh: /usr/include/stdio.h
chsh: /usr/include/sys/types.h
chsh: /usr/include/stat.h
chsh: /usr/include/signal.h
chsh: /usr/include/pwd.h
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
finger: /usr/include/sccs.h
finger: /usr/include/lastlog.h
finger: /usr/include/time.h
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
gets: gets.c
gets: /usr/include/stdio.h
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
last: /usr/include/stat.h
last: /usr/include/utmp.h
lastcomm: lastcomm.c
lastcomm: /usr/include/stdio.h
lastcomm: /usr/include/sys/param.h
lastcomm: /usr/include/sys/acct.h
lastcomm: /usr/include/sys/dir.h
lastcomm: /usr/include/signal.h
lastcomm: /usr/include/pwd.h
lastcomm: /usr/include/stat.h
lastcomm: /usr/include/utmp.h
lastcomm: /usr/include/struct.h
leave: leave.c
leave: /usr/include/stdio.h
leave: /usr/include/utmp.h
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
num: num.c
num: /usr/include/stdio.h
printenv: printenv.c
prmail: prmail.c
prmail: /usr/include/pwd.h
prmail: /usr/include/sys/types.h
prmail: /usr/include/sys/stat.h
pti: pti.c
reset: reset.c
reset: /usr/include/sgtty.h
sccs: sccs.c
sccs: /usr/include/stdio.h
sccs: /usr/include/sys/param.h
sccs: /usr/include/sys/stat.h
sccs: /usr/include/dir.h
sccs: /usr/include/errno.h
sccs: /usr/include/signal.h
sccs: /usr/include/sysexits.h
sccs: /usr/include/pwd.h
soelim: soelim.c
soelim: /usr/include/stdio.h
ssp: ssp.c
ssp: /usr/include/stdio.h
strings: strings.c
strings: /usr/include/stdio.h
strings: /usr/include/a.out.h
strings: /usr/include/ctype.h
symorder: symorder.c
symorder: /usr/include/stdio.h
symorder: /usr/include/pagsiz.h
symorder: /usr/include/sys/types.h
symorder: /usr/include/stat.h
symorder: /usr/include/a.out.h
tail: tail.c
tail: /usr/include/stdio.h
tail: /usr/include/ctype.h
tail: /usr/include/sys/types.h
tail: /usr/include/sys/stat.h
tail: /usr/include/errno.h
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
vmstat: /usr/include/sundev/mbvar.h
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
whereis: /usr/include/dir.h
whoami: whoami.c
whoami: /usr/include/pwd.h
xstr: xstr.c
xstr: /usr/include/stdio.h
xstr: /usr/include/ctype.h
xstr: /usr/include/sys/types.h
xstr: /usr/include/signal.h
what: what.c
what: /usr/include/stdio.h
wc: wc.c
wc: /usr/include/stdio.h
yes: yes.c
clear: clear.c
clear: /usr/include/stdio.h
clear: /usr/include/sgtty.h
lock: lock.c
lock: /usr/include/stdio.h
lock: /usr/include/sys/types.h
lock: /usr/include/sys/stat.h
lock: /usr/include/signal.h
lock: /usr/include/sgtty.h
script: script.c
script: /usr/include/stdio.h
script: /usr/include/signal.h
script: /usr/include/sys/types.h
script: /usr/include/sys/stat.h
script: /usr/include/sys/ioctl.h
script: /usr/include/sgtty.h
script: /usr/include/time.h
script: /usr/include/wait.h
ul: ul.c
ul: /usr/include/stdio.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
