#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
RSRCS=	retest.c regexp.c
ROBJS=	retest.o regexp.o
VSRCS=	regexp.c vfontedpr.c vgrindefs.c
VOBJS=	regexp.o vfontedpr.o vgrindefs.o

all: retest vfontedpr

vfontedpr: ${VOBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${VOBJS}

retest: ${ROBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${ROBJS}

clean: FRC
	rm -f ${OBJS} ${ROBJS} ${VOBJS} core retest vfontedpr

depend: FRC
	mkdep ${CFLAGS} ${RSRCS} ${VSRCS}

install: FRC
	install -s -o bin -g bin -m 755 vfontedpr ${DESTDIR}/usr/lib/vfontedpr
	install -c -o bin -g bin -m 755 vgrind.sh ${DESTDIR}/usr/ucb/vgrind
	install -c -o bin -g bin -m 755 vgrindefs.src ${DESTDIR}/usr/lib/vgrindefs
	install -c -o bin -g bin -m 755 tmac.vgrind ${DESTDIR}/usr/lib/tmac/tmac.vgrind

lint: FRC
	lint ${CFLAGS} ${RSRCS}
	lint ${CFLAGS} ${VSRCS}

tags: FRC
	ctags ${RSRCS}
	ctags -a ${VSRCS}
	sort -o tags tags

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

retest.o: retest.c /usr/include/ctype.h
regexp.o: regexp.c /usr/include/ctype.h regexp.c /usr/include/ctype.h
vfontedpr.o: vfontedpr.c /usr/include/ctype.h /usr/include/stdio.h
vfontedpr.o: /usr/include/sys/types.h /usr/include/sys/stat.h
vgrindefs.o: vgrindefs.c /usr/include/ctype.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
