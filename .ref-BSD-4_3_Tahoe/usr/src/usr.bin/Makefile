#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.43.1.1	(Berkeley)	6/8/88
#
DESTDIR=
CFLAGS=	-O
LIBC=	/lib/libc.a

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	at calendar dc diction diff3 efl eqn f77 find graph learn lex lint \
	m4 plot prof ratfor refer roff spell struct tbl tip uucp xsend yacc

# Shell scripts that need only be installed and are never removed.
#
SCRIPT=	lorder mkdep nohup

# C source that lives in the current directory
#
SRCS=	ar11.c basename.c bc.c cal.c cb.c checkeq.c col.c comm.c crypt.c \
	deroff.c file.c install.c join.c look.c mesg.c ptx.c \
	ranlib.c rev.c sleep.c sort.c spline.c split.c sum.c tabs.c \
	tc.c tk.c touch.c tr.c tsort.c tty.c uniq.c units.c

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	ar11 basename bc cal cb checkeq col comm crypt deroff \
	file join look mesg ptx ranlib rev sleep sort spline split \
	sum tabs tc tk touch tr tsort tty uniq units

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	xinstall

# Programs that get installed with special permissions
#
PERM=	iostat

all: ${SUBDIR} ${STD} ${PERM} ${NSTD}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

bc: bc.c ${LIBC}
${STD} ${PERM}: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

install: FRC
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${SCRIPT}; do \
		(install -c -o bin -g bin -m 555 $$i.sh ${DESTDIR}/usr/bin/$$i); done
	install -s -o bin -g bin -m 755 ${STD} ${DESTDIR}/usr/bin
	install -s -o bin -g bin -m 755 install ${DESTDIR}/usr/bin/install
	install -s -o bin -g kmem -m 2755 iostat ${DESTDIR}/usr/bin/iostat
	install -c -o bin -g bin -m 755 units.lib ${DESTDIR}/usr/lib/units

clean: FRC
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done
	rm -f a.out core *.s *.o bc.c y.tab.c install ${STD} ${NSTD} ${PERM}

depend: ldepend
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done

ldepend: ${SRCS} FRC
	mkdep -p ${CFLAGS} ${SRCS}

FRC:

# Files listed in ${NSTD} have explicit make lines given below.

xinstall:
	${CC} ${CFLAGS} install.c -o install

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

ar11: ar11.c /usr/include/signal.h /usr/include/machine/trap.h
ar11: /usr/include/stdio.h /usr/include/sys/types.h /usr/include/sys/stat.h
basename: basename.c
bc: bc.c /usr/include/stdio.h /usr/include/varargs.h
cal: cal.c /usr/include/sys/types.h /usr/include/time.h /usr/include/stdio.h
cb: cb.c /usr/include/stdio.h
checkeq: checkeq.c /usr/include/stdio.h
col: col.c /usr/include/stdio.h
comm: comm.c /usr/include/stdio.h
crypt: crypt.c /usr/include/stdio.h
deroff: deroff.c /usr/include/stdio.h
file: file.c /usr/include/sys/param.h /usr/include/sys/types.h
file: /usr/include/signal.h /usr/include/machine/trap.h
file: /usr/include/machine/machparam.h /usr/include/machine/endian.h
file: /usr/include/sys/stat.h /usr/include/stdio.h /usr/include/ctype.h
file: /usr/include/a.out.h /usr/include/sys/exec.h
install: install.c /usr/include/sys/param.h /usr/include/sys/types.h
install: /usr/include/signal.h /usr/include/machine/trap.h
install: /usr/include/machine/machparam.h /usr/include/machine/endian.h
install: /usr/include/sys/stat.h /usr/include/sys/file.h /usr/include/a.out.h
install: /usr/include/sys/exec.h /usr/include/grp.h /usr/include/pwd.h
install: /usr/include/stdio.h /usr/include/ctype.h
join: join.c /usr/include/stdio.h
look: look.c /usr/include/sys/types.h /usr/include/sys/file.h
look: /usr/include/sys/stat.h /usr/include/stdio.h /usr/include/ctype.h
mesg: mesg.c /usr/include/sys/types.h /usr/include/sys/stat.h
mesg: /usr/include/stdio.h
ptx: ptx.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/signal.h
ptx: /usr/include/machine/trap.h
ranlib: ranlib.c /usr/include/sys/types.h /usr/include/ar.h
ranlib: /usr/include/ranlib.h /usr/include/a.out.h /usr/include/sys/exec.h
ranlib: /usr/include/stdio.h
rev: rev.c /usr/include/stdio.h
sleep: sleep.c /usr/include/stdio.h
sort: sort.c /usr/include/sys/param.h /usr/include/sys/types.h
sort: /usr/include/signal.h /usr/include/machine/trap.h
sort: /usr/include/machine/machparam.h /usr/include/machine/endian.h
sort: /usr/include/stdio.h /usr/include/ctype.h /usr/include/signal.h
sort: /usr/include/sys/stat.h
spline: spline.c /usr/include/stdio.h /usr/include/math.h
split: split.c /usr/include/sys/param.h /usr/include/sys/types.h
split: /usr/include/signal.h /usr/include/machine/trap.h
split: /usr/include/machine/machparam.h /usr/include/machine/endian.h
split: /usr/include/sys/file.h /usr/include/stdio.h /usr/include/ctype.h
sum: sum.c /usr/include/stdio.h
tabs: tabs.c /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
tabs: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tc: tc.c /usr/include/signal.h /usr/include/machine/trap.h /usr/include/stdio.h
tk: tk.c /usr/include/stdio.h /usr/include/signal.h /usr/include/machine/trap.h
touch: touch.c /usr/include/stdio.h /usr/include/sys/types.h
touch: /usr/include/sys/file.h /usr/include/sys/stat.h
tr: tr.c /usr/include/stdio.h
tsort: tsort.c /usr/include/stdio.h
tty: tty.c /usr/include/stdio.h
uniq: uniq.c /usr/include/stdio.h /usr/include/ctype.h
units: units.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
