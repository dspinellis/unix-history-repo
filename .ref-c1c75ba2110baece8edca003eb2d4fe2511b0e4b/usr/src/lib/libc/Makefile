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
#	@(#)Makefile	4.34 (Berkeley) %G%
#
# Compatibility routines are kept in directories with a prefixing
# ``compat'' (so they all sort together).
# 
# The C run-time startup code is always machine dependent and expected
# to be located in ${MACHINE}/csu
#
# There are two possible methods of doing host name look up.  They are
# to use the name server or /etc/hosts. HOSTLOOKUP defines which method
# is to be compiled into libc  :
#								     
#	defining HOSTLOOKUP to be "named", compiles the         
#	host lookup routines that use the BIND name server.
#
#	defining HOSTLOOKUP to be "hosttable", compiles
#	the host lookup routines that use /etc/hosts.
#
#HOSTLOOKUP=	hosttable
HOSTLOOKUP=	named
#
# The system has two monitoring schemes, utilizing either ``prof''
# or ``gprof''. DFLMON determines the default profiler; mon.o
# specifies that ``prof'' is the default, gmon.o specifies that
# ``gprof'' is the default.
#DFLMON=	gmon.o
DFLMON=		mon.o
#
# All library object contain sccsid strings by default; they may be
# excluded as a space-saving measure.  To produce a library that does
# not contain these strings, remove -DLIBC_SCCS and -DSYSLIBC_SCCS
# from DEFS below.  To remove these strings from just the system call
# stubs, remove -DSYSLIBC_SCCS from DEFS below.
#DEFS=
DEFS="-DLIBC_SCCS -DSYSLIBC_SCCS"

SUBDIR=	${ALL} net/hosttable net/named
ONE=	compat-4.1 compat-4.3 compat-sys5 gen inet net ns stdio
TWO=	compat-4.1/compat-4.1.${MACHINE} gen/gen.${MACHINE} \
	net/net.${MACHINE} net/${HOSTLOOKUP} stdio/stdio.${MACHINE} \
	compat-sys5/compat-sys5.${MACHINE} ${MACHINE}/csu ${MACHINE}/sys
ALL=	${ONE} ${TWO}
TAGSFILE=tags

libc.a libc_p.a: mkdir ${ALL} tags
	ls library/*.o | sort -t/ +1 > t1
	ar cr libc.a `lorder \`cat t1\` | tsort` ${MACHINE}/csu/${DFLMON}
	ar ma findiop.o libc.a exit.o
	ar ma exit.o libc.a fakcu.o
	ls profiled/*.o | sort -t/ +1 > t1
	ar cr libc_p.a `lorder \`cat t1\` | tsort` ${MACHINE}/csu/${DFLMON}
	ar ma findiop.o libc_p.a exit.o
	ar ma exit.o libc_p.a fakcu.o
	rm -rf t1 library profiled

mkdir: FRC
	rm -rf library profiled
	mkdir library profiled

${ALL}: FRC
	cd $@; make ${MFLAGS} DEFS=${DEFS} link

install: 
	install -o bin -g bin -m 644 libc.a ${DESTDIR}/lib/libc.a
	ranlib ${DESTDIR}/lib/libc.a
	install -o bin -g bin -m 644 libc_p.a ${DESTDIR}/usr/lib/libc_p.a
	ranlib ${DESTDIR}/usr/lib/libc_p.a
	cd ${MACHINE}/csu; make DESTDIR=${DESTDIR} install
	install -o bin -g bin -m 444 ${TAGSFILE} ${DESTDIR}/usr/lib/${TAGSFILE}

tags:
	for i in ${ONE}; do \
		(cd $$i; make ${MFLAGS} TAGSFILE=../${TAGSFILE} tags); \
	done
	for i in ${TWO}; do \
		(cd $$i; make ${MFLAGS} TAGSFILE=../../${TAGSFILE} tags); \
	done
	/usr/bin/sort -o ${TAGSFILE} ${TAGSFILE}

clean:
	for i in ${SUBDIR}; \
		do (cd $$i; make ${MFLAGS} clean); done
	rm -rf libc.a libc_p.a library profiled

depend:
	for i in ${SUBDIR}; \
		do (cd $$i; make ${MFLAGS} DEFS=${DEFS} depend); done

FRC:

