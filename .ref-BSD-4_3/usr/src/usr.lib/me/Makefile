#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.1 (Berkeley) 5/21/86
#
#  Makefile for -me macros
#
#	This makefile strips comments from the source files and
#	installs them in the expected directories.
#
DESTDIR=
LIB =	/usr/lib/me

SRCS =	tmac.e acm.me chars.me deltext.me eqn.me \
	float.me footnote.me index.me local.me null.me \
	refer.me sh.me tbl.me thesis.me
GET =	sccs get

all: revisions $(SRCS)

install: revisions $(SRCS)
	-if [ ! -d ${DESTDIR}${LIB} ]; then \
		rm -f ${DESTDIR}${LIB}; \
		mkdir ${DESTDIR}${LIB}; \
	fi
	-for i in ${SRCS}; do \
		sed -e '/%beginstrip%/{' \
		    -e 'h' \
		    -e 's/.*/.\\" This version has had comments stripped; an unstripped version is available./p' \
		    -e 'g' \
		    -e '}' \
		    -e '/%beginstrip%/,$$s/[. 	][ 	]*\\".*//' \
		    -e '/^$$/d' \
		    -e '/\\n@/d' $$i > _mac_tmp_; \
		install -m 0444 _mac_tmp_ ${DESTDIR}${LIB}/$$i; \
	done
	mv ${DESTDIR}${LIB}/tmac.e ${DESTDIR}/usr/lib/tmac/tmac.e
	install -c -m 0444 revisions ${DESTDIR}${LIB}/revisions

clean:

revisions $(SRCS):
	$(GET) SCCS/s.$@
