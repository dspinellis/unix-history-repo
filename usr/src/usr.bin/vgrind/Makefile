#
# Copyright (c) 1987 The Regents of the University of California.
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
#	@(#)Makefile	5.5 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
RSRCS=	retest.c regexp.c
ROBJS=	retest.o regexp.o
VSRCS=	regexp.c vfontedpr.c vgrindefs.c
VOBJS=	regexp.o vfontedpr.o vgrindefs.o
MAN=	vgrind.0 vgrindefs.0

all: retest vfontedpr

vfontedpr: ${VOBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${VOBJS}

retest: ${ROBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${ROBJS}

clean:
	rm -f ${OBJS} ${ROBJS} ${VOBJS} core retest vfontedpr

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${RSRCS} ${VSRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 vfontedpr ${DESTDIR}/usr/libexec
	install -c -o bin -g bin -m 755 vgrind.sh ${DESTDIR}/usr/pgrm/vgrind
	install -c -o bin -g bin -m 755 vgrindefs.src \
	    ${DESTDIR}/usr/share/misc/vgrindefs
	install -c -o bin -g bin -m 755 tmac.vgrind \
	    ${DESTDIR}/usr/share/tmac/tmac.vgrind
	install -c -o bin -g bin -m 444 vgrind.0 ${DESTDIR}/usr/man/cat1
	install -c -o bin -g bin -m 444 vgrindefs.0 ${DESTDIR}/usr/man/cat5

lint: ${SRCS}
	lint ${CFLAGS} ${RSRCS}
	lint ${CFLAGS} ${VSRCS}

tags: ${SRCS}
	ctags ${RSRCS}
	ctags -a ${VSRCS}
	sort -o tags tags

FRC:
