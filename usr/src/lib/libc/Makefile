#
# Copyright (c) 1988 Regents of the University of California.
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
#	@(#)Makefile	4.35 (Berkeley) %G%

# The C run-time startup code is machine dependent and found in csu.

# The system has two monitoring schemes, utilizing either ``prof''
# or ``gprof''. DFLMON determines the default profiler; mon.o
# specifies that ``prof'' is the default, gmon.o specifies that
# ``gprof'' is the default.
#DFLMON=	gmon.o
DFLMON=		mon.o

# All library objects contain sccsid strings by default; they may be
# excluded as a space-saving measure.  To produce a library that does
# not contain these strings, remove -DLIBC_SCCS and -DSYSLIBC_SCCS
# from DEFS below.  To remove these strings from just the system call
# stubs, remove -DSYSLIBC_SCCS from DEFS below.
#DEFS=
DEFS="-DLIBC_SCCS -DSYSLIBC_SCCS"

CSU=	csu
SRCDIR=	compat-43 gen net stdio string sys sys5
SUBDIR=	${CSU} ${SRCDIR}
TAGSFILE=tags
TPATH=`pwd`/${TAGSFILE}

all: library ${SUBDIR}
	cd library; ls *.o | sort > t1; \
	ar cr libc.a `lorder \`cat t1\` | tsort` ../csu/${MACHINE}/${DFLMON}; \
	ar ma findiop.o libc.a exit.o; \
	ar ma exit.o libc.a fakcu.o; \
	rm -f t1
	cd profiled; ls *.o | sort > t1; \
	ar cr libc_p.a `lorder \`cat t1\` | tsort` ../csu/${MACHINE}/${DFLMON}; \
	ar ma findiop.o libc_p.a exit.o; \
	ar ma exit.o libc_p.a fakcu.o; \
	rm -f t1

${SUBDIR}: FRC
	cd $@; make ${MFLAGS} DEFS=${DEFS}

clean:
	for i in ${SUBDIR}; \
		do (cd $$i; make ${MFLAGS} clean); done
	rm -rf library/libc.a profiled/libc_p.a

cleandir: clean
	for i in ${SUBDIR}; \
		do (cd $$i; make ${MFLAGS} cleandir); done

depend:
	for i in ${SUBDIR}; \
		do (cd $$i; make ${MFLAGS} DEFS=${DEFS} $@); done

install: tags
	install -o bin -g bin -m 644 library/libc.a ${DESTDIR}/usr/lib/libc.a
	ranlib ${DESTDIR}/usr/lib/libc.a
	install -o bin -g bin -m 644 profiled/libc_p.a ${DESTDIR}/usr/lib/libc_p.a
	ranlib ${DESTDIR}/usr/lib/libc_p.a
	for i in ${SUBDIR}; \
		do (cd $$i; make DEFS=${DEFS} $@); done
	install -o bin -g bin -m 444 ${TAGSFILE} ${DESTDIR}/usr/libdata

tags:
	for i in ${SUBDIR}; do \
		(cd $$i; make TAGSFILE=tags tags); done
	rm -f tags
	for i in ${SUBDIR}; do \
		cat $$i/tags >> tags; done
	sort -o tags tags

FRC:
