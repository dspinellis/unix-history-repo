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
#	@(#)Makefile	5.5 (Berkeley) %G%
#
DIRS=	man3 man3f man4 man4/vax man4/tahoe man5 man7 man8 man8/vax

all: FRC
	@-for i in ${DIRS}; do \
		(echo "==> $$i"; \
		    cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} $@); \
	done

install: FRC
	install -c -o bin -g bin -m 444 makewhatis.sed \
		${DESTDIR}/usr/share/man/makewhatis.sed
	@-for i in ${DIRS}; do \
		(echo "==> $$i"; \
		    cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	for file in `find /usr/share/man -type f -name '*.0' -print`; do \
		sed -n -f /usr/share/man/makewhatis.sed $$file; \
	done | sort -u > whatis.db
	install -o bin -g bin -m 444 whatis.db ${DESTDIR}/usr/share/man

clean cleandir depend lint tags:
	@-for i in ${DIRS}; do \
		(echo "==> $$i"; \
		    cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} $@); done

FRC:
