
#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.14 (Berkeley) %G%
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
# All files contain sccsid strings. To compile a library that does
# not include these strings one must add -Dlint to DEFS below.
#
#*********************************************************************
#=====================================================================
#							
# There are two possible methods of doing host name look up.  They are
# to use the nameserver or /etc/hosts.
#
#	HOSTLOOKUP defines which method is to be compiled into libc  
#								     
#		defining HOSTLOOKUP to be "named", compiles the         
#		routines which the BIND nameserver uses into libc    
#---------------------------------------------------------------------
#HOSTLOOKUP=	named
#---------------------------------------------------------------------
#		defining HOSTLOOKUP to "hosttable", compiles
#		the routines which use the old method of
#		hostname lookup. (ie /etc/hosts )
#---------------------------------------------------------------------
HOSTLOOKUP=	hosttable
DESTDIR=
INSTALL=install -m 644
DFLMON=	mon.o
MACHINE=vax
DEFS=
COMPAT=	compat-4.1 compat-sys5
ALL=	gen inet net stdio ${MACHINE} ${COMPAT}
TAGSFILE=tags

libc.a libc_p.a: ${MACHINE}/csu ${ALL} net/${HOSTLOOKUP}
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
	${INSTALL} libc.a ${DESTDIR}/lib/libc.a
	ranlib ${DESTDIR}/lib/libc.a
	${INSTALL} libc_p.a ${DESTDIR}/usr/lib/libc_p.a
	ranlib ${DESTDIR}/usr/lib/libc_p.a
	cd ${MACHINE}/csu; make DESTDIR=${DESTDIR} install

tags:
	for i in ${ALL}; do \
		(cd $$i; make ${MFLAGS} TAGSFILE=../${TAGSFILE} tags); \
	cd net/${HOSTLOOKUP}; \
		make ${MFLAGS} TAGSFILE=../../${TAGSFILE} tags
	done

clean:
	for i in ${MACHINE}/csu ${ALL} net/${HOSTLOOKUP}; \
		do (cd $$i; make ${MFLAGS} clean); done
	rm -f t1 tmp/*.o
	-rmdir tmp
