#
# Copyright (c) 1989 The Regents of the University of California.
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
#	@(#)Makefile	5.2 (Berkeley) %G%
#
MSLIB=	/usr/share/ms
TMLIB=	/usr/share/tmac
MSSRCS=	acc.ms cov.ms eqn.ms ref.ms tbl.ms ths.ms toc.ms
TMSRCS=	tmac.os tmac.s

all clean cleandir depend lint tags:

install:
	-[ -d ${DESTDIR}${MSLIB} ] || mkdir ${DESTDIR}${MSLIB}
	chown bin.bin ${DESTDIR}${MSLIB}
	chmod 755 ${DESTDIR}${MSLIB}
	install -c -o bin -g bin -m 444 ${MSSRCS} ${DESTDIR}${MSLIB}
	install -c -o bin -g bin -m 444 ${TMSRCS} ${DESTDIR}${TMLIB}
