#	Makefile	4.39	85/10/11
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	at calendar dc diction diff3 efl eqn f77 find graph learn lex lint \
	m4 neqn nroff plot prof ratfor refer spell struct tbl tip troff uucp \
	xsend yacc

# Shell scripts that need only be installed and are never removed.
#
SCRIPT=	install lorder nohup

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	ar11 basename cal cb checkeq col comm crypt deroff \
	fgrep file join look mesg ptx ranlib \
	rev sleep sort spline split sum tabs tc tk touch tr tsort tty \
	uniq units 

# C programs that live in the current directory and need explicit make lines.
# (make depend has special rules for these files)
#
NSTD=	bc egrep

# Programs that must run set-group-id kmem.
#
KMEM=	iostat

all:	${SUBDIR} ${STD} ${NSTD} ${KMEM}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

FRC:

bc egrep:
	${YACC} ${YFLAGS} $@.y
	${CC} -c ${CFLAGS} y.tab.c
	mv y.tab.o $@.o
	${CC} -o $@ $@.o
	rm -f $@.o

${STD} ${KMEM}:
	${CC} ${CFLAGS} -o $@ $@.c

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${SCRIPT}; do \
		(install -c -m 555 $$i.sh ${DESTDIR}/usr/bin/$$i); done
	-for i in ${STD} ${NSTD}; do \
		(install -s $$i ${DESTDIR}/usr/bin/$$i); done
	-for i in ${KMEM}; do \
		(install -s -g kmem -m 2755 $$i ${DESTDIR}/usr/bin/$$i); done
	install -c units.lib ${DESTDIR}/usr/lib/units

clean:
	rm -f a.out core *.s *.o y.tab.c
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done
	rm -f ${STD} ${NSTD} ${KMEM}

# beware of the non-standard stuff for bc and egrep
depend:
	for i in ${STD} ${KMEM}; do \
	    cc -M ${INCPATH} $$i.c | sed 's/\.o//' | \
	    awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
	for i in bc egrep; do \
	    cc -M ${INCPATH} $$i.y | sed 's/\.o//' | \
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

# DO NOT DELETE THIS LINE -- make depend uses it

ar11: ar11.c /usr/include/signal.h /usr/include/stdio.h
ar11: /usr/include/sys/types.h /usr/include/sys/stat.h
basename: basename.c /usr/include/stdio.h
cal: cal.c
cb: cb.c /usr/include/stdio.h
checkeq: checkeq.c /usr/include/stdio.h
col: col.c /usr/include/stdio.h
comm: comm.c /usr/include/stdio.h
crypt: crypt.c /usr/include/stdio.h
deroff: deroff.c /usr/include/stdio.h
fgrep: fgrep.c /usr/include/stdio.h /usr/include/ctype.h
fgrep: /usr/include/sys/param.h /usr/include/machine/machparam.h
fgrep: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
file: file.c /usr/include/sys/param.h /usr/include/machine/machparam.h
file: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
file: /usr/include/stdio.h /usr/include/ctype.h /usr/include/a.out.h
file: /usr/include/sys/exec.h
join: join.c /usr/include/stdio.h
look: look.c /usr/include/stdio.h /usr/include/ctype.h
mesg: mesg.c /usr/include/stdio.h /usr/include/sys/types.h
mesg: /usr/include/sys/stat.h
ptx: ptx.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/signal.h
ranlib: ranlib.c /usr/include/sys/types.h /usr/include/ar.h
ranlib: /usr/include/ranlib.h /usr/include/a.out.h /usr/include/sys/exec.h
ranlib: /usr/include/stdio.h
rev: rev.c /usr/include/stdio.h
sleep: sleep.c
sort: sort.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/signal.h
sort: /usr/include/sys/types.h /usr/include/sys/stat.h
spline: spline.c /usr/include/stdio.h /usr/include/math.h
split: split.c /usr/include/stdio.h
sum: sum.c /usr/include/stdio.h
tabs: tabs.c /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
tabs: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tc: tc.c /usr/include/signal.h /usr/include/stdio.h
tk: tk.c /usr/include/stdio.h /usr/include/signal.h
touch: touch.c /usr/include/stdio.h /usr/include/sys/types.h
touch: /usr/include/sys/stat.h
tr: tr.c /usr/include/stdio.h
tsort: tsort.c /usr/include/stdio.h
tty: tty.c
uniq: uniq.c /usr/include/stdio.h /usr/include/ctype.h
units: units.c /usr/include/stdio.h
iostat: iostat.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/nlist.h
iostat: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/file.h
iostat: /usr/include/sys/buf.h /usr/include/sys/dk.h
iostat: /usr/include/vaxuba/ubavar.h /usr/include/vaxmba/mbavar.h
bc: bc.y /usr/include/stdio.h
egrep: egrep.y /usr/include/stdio.h /usr/include/sys/types.h
egrep: /usr/include/sys/stat.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
