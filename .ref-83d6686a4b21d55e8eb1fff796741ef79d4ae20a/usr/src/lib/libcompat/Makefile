#
# Copyright (c) 1989 Regents of the University of California.
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
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.1 (Berkeley) %G%

# All library objects contain sccsid strings by default; they may be
# excluded as a space-saving measure.  To produce a library that does
# not contain these strings, remove -DLIBC_SCCS and -DSYSLIBC_SCCS
# from DEFS below.  To remove these strings from just the system call
# stubs, remove -DSYSLIBC_SCCS from DEFS below.
#DEFS=
DEFS="-DLIBC_SCCS -DSYSLIBC_SCCS"

SUBDIR=	4.1 4.3
TAGSFILE=tags

all: ${SUBDIR}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS} DEFS=${DEFS}

clean:
	for i in ${SUBDIR}; \
		do (cd $$i; make ${MFLAGS} clean); done
	rm -rf libcompat.a libcompat_p.a library profiled

cleandir: clean
	for i in ${SUBDIR}; \
		do (cd $$i; make ${MFLAGS} cleandir); done

depend tags:
	for i in ${SUBDIR}; \
		do (cd $$i; make ${MFLAGS} DEFS=${DEFS} $@); done

install: tags
	rm -rf library profiled
	mkdir library profiled
	for i in ${SUBDIR}; \
		do (cd $$i; make ${MFLAGS} DEFS=${DEFS} $@); done
	ls library/*.o | sort -t/ +1 > t1
	ar cr libcompat.a `lorder \`cat t1\` | tsort` csu/${MACHINE}/${DFLMON}
	ls profiled/*.o | sort -t/ +1 > t1
	ar cr libcompat_p.a `lorder \`cat t1\` | tsort` csu/${MACHINE}/${DFLMON}
	rm -rf t1 library profiled
	install -o bin -g bin -m 644 libcompat.a ${DESTDIR}/usr/lib
	ranlib ${DESTDIR}/lib/libcompat.a
	install -o bin -g bin -m 644 libcompat_p.a ${DESTDIR}/usr/lib
	ranlib ${DESTDIR}/usr/lib/libcompat_p.a

FRC:
