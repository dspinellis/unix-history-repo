#
# Copyright (c) 1987 Regents of the University of California.
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
#	@(#)Makefile	5.4 (Berkeley) %G%
#
DIRS=	man1 man2 man3 man3f man4 man4/vax man4/tahoe man5 man7 man8 man8/vax

all: scriptinstall FRC
	install -c -o bin -g bin -m 444 makewhatis.sed \
		${DESTDIR}/usr/man/makewhatis.sed
	for i in ${DIRS}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} $@); \
	done


install: FRC
	for i in ${DIRS}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	for file in `find /usr/man -type f -name '*.0' -print`; do \
		sed -n -f /usr/man/makewhatis.sed $$file; \
	done | sort -u > whatis.db
	install -o bin -g bin -m 444 whatis.db ${DESTDIR}/usr/man/whatis
	install -c -o bin -g bin -m 444 man.template ${DESTDIR}/usr/man/man.template

clean cleandir depend lint tags:
	for i in ${DIRS}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} $@); done

FRC:
