#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.27 (Berkeley) %G%
#
#
# The system has two monitoring schemes, utilizing either ``prof''
# or ``gprof''. DFLMON determines the default profiler; mon.o
# specifies that ``prof'' is the default, gmon.o specifies that
# ``gprof'' is the default.
#
# Machine dependent routines are located in a subtree which parallels
# the top directories.  This subtree is identified by the machine name.
#
# Compatibility routines are kept in directories with a prefixing
# ``compat'' (so they all sort together).
# 
# The C run-time startup code is always machine dependent and expected
# to be located in ${MACHINE}/csu
#
# All files contain sccsid strings, but these are not compiled into
# library objects by default, as a space-saving measure.  To produce
# a library that contains these strings in every object except
# system call stubs, add -DLIBC_SCCS to DEFS below; to put these
# strings into system call stubs, use -DSYSLIBC_SCCS.
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
DESTDIR=
DFLMON=	mon.o
DEFS=
COMPAT=	compat-4.1 compat-sys5
ALL=	gen inet net ns stdio ${MACHINE} ${COMPAT}
TAGSFILE=tags

libc.a libc_p.a: ${MACHINE}/csu ${ALL} net/${HOSTLOOKUP}
	rm -rf t1 tmp
	-mkdir tmp
	for i in ${ALL}; do (cd tmp; ar x ../$$i/$${i}lib); done
	cd tmp; ar x ../net/${HOSTLOOKUP}/hostlib
	ls tmp/*.o | sort -t/ +1 > t1
	ar cr libc.a `lorder \`cat t1\` | tsort` ${MACHINE}/csu/${DFLMON}
	ar ma findiop.o libc.a exit.o
	ar ma exit.o libc.a fakcu.o
	rm -f t1 tmp/*
	for i in ${ALL}; do (cd tmp; ar x ../$$i/$${i}lib_p); done
	cd tmp; ar x ../net/${HOSTLOOKUP}/hostlib_p
	ls tmp/*.o | sort -t/ +1 > t1
	ar cr libc_p.a `lorder \`cat t1\` | tsort` ${MACHINE}/csu/${DFLMON}
	ar ma findiop.o libc_p.a exit.o
	ar ma exit.o libc_p.a fakcu.o
	rm -rf t1 tmp

${MACHINE}/csu ${ALL} net/${HOSTLOOKUP}: FRC
	cd $@; make ${MFLAGS} DEFS=${DEFS}

FRC:

install: 
	install -o bin -g bin -m 644 libc.a ${DESTDIR}/lib/libc.a
	ranlib ${DESTDIR}/lib/libc.a
	install -o bin -g bin -m 644 libc_p.a ${DESTDIR}/usr/lib/libc_p.a
	ranlib ${DESTDIR}/usr/lib/libc_p.a
	cd ${MACHINE}/csu; make DESTDIR=${DESTDIR} install

tags:
	for i in ${ALL}; do \
		(cd $$i; make ${MFLAGS} TAGSFILE=../${TAGSFILE} tags); \
	done
	/usr/bin/sort -o ${TAGSFILE} ${TAGSFILE}

clean:
	for i in ${MACHINE}/csu ${ALL} net/hosttable net/named; \
		do (cd $$i; make ${MFLAGS} clean); done
	rm -rf tmp t1

depend:
	for i in ${ALL} net/hosttable net/named; \
		do (cd $$i; make ${MFLAGS} DEFS=${DEFS} depend); done
