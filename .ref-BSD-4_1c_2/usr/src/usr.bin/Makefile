#	@(#)Makefile	4.16	(Berkeley)	2/27/83
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	calendar dc diction diff3 efl eqn f77 graph learn lex lint m4 neqn \
	plot prof ratfor refer struct tbl tip troff uucp xsend yacc

# Shell scripts that need only be installed and are never removed.
#
SCRIPT=	install lookbib lorder man nohup

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	ar11 basename bc cal cb checkeq col comm crypt deroff \
	fgrep file find iostat join look mesg ptx ranlib \
	rev sleep sort spline split sum tabs tc tk touch tr tsort tty \
	uniq units 

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	egrep

all:	${SUBDIR} ${STD} ${NSTD}

${SUBDIR}: /tmp
	cd $@; make ${MFLAGS}

egrep:	egrep.o
	cc -o egrep egrep.o
	rm -f egrep.o

${STD}:
	cc ${CFLAGS} -o $@ $@.c

install:
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	for i in ${SCRIPT}; do (install -c $$i.sh ${DESTDIR}/usr/bin/$$i); done
	for i in ${STD} ${NSTD}; do (install $$i ${DESTDIR}/usr/bin/$$i); done
	install -c units.lib ${DESTDIR}/usr/lib/units

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

# DO NOT DELETE THIS LINE -- make depend uses it

ar11: ar11.c
ar11: /usr/include/signal.h
ar11: /usr/include/stdio.h
ar11: /usr/include/sys/types.h
ar11: /usr/include/sys/stat.h
ar11: /usr/include/signal.h
basename: basename.c
basename: /usr/include/stdio.h
bc: bc.c
bc: /usr/include/stdio.h
cal: cal.c
cb: cb.c
cb: /usr/include/stdio.h
checkeq: checkeq.c
checkeq: /usr/include/stdio.h
col: col.c
col: /usr/include/stdio.h
comm: comm.c
comm: /usr/include/stdio.h
crypt: crypt.c
crypt: /usr/include/stdio.h
deroff: deroff.c
deroff: /usr/include/stdio.h
fgrep: fgrep.c
fgrep: /usr/include/stdio.h
fgrep: /usr/include/ctype.h
file: file.c
file: /usr/include/pagsiz.h
file: /usr/include/sys/types.h
file: /usr/include/stat.h
file: /usr/include/stdio.h
file: /usr/include/ctype.h
file: /usr/include/a.out.h
find: find.c
find: /usr/include/stdio.h
find: /usr/include/sys/param.h
find: /usr/include/sys/dir.h
find: /usr/include/sys/stat.h
find: /usr/include/signal.h
iostat: iostat.c
iostat: /usr/include/stdio.h
iostat: /usr/include/nlist.h
iostat: /usr/include/sys/types.h
iostat: /usr/include/sys/buf.h
iostat: /usr/include/sys/dk.h
iostat: /usr/include/vaxuba/ubavar.h
iostat: /usr/include/vaxmba/mbavar.h
join: join.c
join: /usr/include/stdio.h
look: look.c
look: /usr/include/stdio.h
look: /usr/include/ctype.h
mesg: mesg.c
mesg: /usr/include/stdio.h
mesg: /usr/include/sys/types.h
mesg: /usr/include/sys/stat.h
ptx: ptx.c
ptx: /usr/include/stdio.h
ptx: /usr/include/ctype.h
ptx: /usr/include/signal.h
ranlib: ranlib.c
ranlib: /usr/include/sys/types.h
ranlib: /usr/include/ar.h
ranlib: /usr/include/ranlib.h
ranlib: /usr/include/pagsiz.h
ranlib: /usr/include/a.out.h
ranlib: /usr/include/stdio.h
rev: rev.c
rev: /usr/include/stdio.h
sleep: sleep.c
sort: sort.c
sort: /usr/include/stdio.h
sort: /usr/include/ctype.h
sort: /usr/include/signal.h
sort: /usr/include/sys/types.h
sort: /usr/include/sys/stat.h
spline: spline.c
spline: /usr/include/stdio.h
split: split.c
split: /usr/include/stdio.h
sum: sum.c
sum: /usr/include/stdio.h
tabs: tabs.c
tabs: /usr/include/stdio.h
tabs: /usr/include/sgtty.h
tc: tc.c
tc: /usr/include/signal.h
tc: /usr/include/stdio.h
tk: tk.c
tk: /usr/include/stdio.h
tk: /usr/include/signal.h
touch: touch.c
touch: /usr/include/stdio.h
touch: /usr/include/sys/types.h
touch: /usr/include/sys/stat.h
tr: tr.c
tr: /usr/include/stdio.h
tsort: tsort.c
tsort: /usr/include/stdio.h
tty: tty.c
uniq: uniq.c
uniq: /usr/include/stdio.h
uniq: /usr/include/ctype.h
units: units.c
units: /usr/include/stdio.h
egrep: egrep.c
egrep: /usr/include/stdio.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
