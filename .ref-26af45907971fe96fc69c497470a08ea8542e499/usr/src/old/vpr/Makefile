#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.10	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/usr/lib/libc.a
SUBDIR=	vtools vtools/fed man
SHELLS=	rvtroff.sh vprint.sh vtroff.sh vpq.sh vpr.sh vprm.sh
SRCS=	vpac.c vpd.c vpq.c vpr.c vprm.c
OBJS=	vpac.o vpd.o vpq.o vpr.o vprm.o
ALL=	vpac vpd vpq vpr vprm

all: ${ALL} subdir

subdir:
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS}); done

vpac: ${LIBC}
	${CC} -O -o $@ vpac.c
vpd: ${LIBC}
	${CC} -DVERSATEC -O -o $@ vpd.c
vpq: ${LIBC}
	${CC} -O -o $@ vpq.c
vpr: ${LIBC}
	${CC} -O -o $@ vpr.c
vprm: ${LIBC}
	${CC} -O -o $@ vprm.c

clean: FRC
	rm -f ${OBJS} core ${ALL}
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done

depend: FRC ldepend
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done

ldepend:
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} install); done
	install -s -o bin -g bin -m 755 vpac ${DESTDIR}/usr/old/vpac
	install -s -o bin -g bin -m 755 vpd ${DESTDIR}/usr/lib/vpd
	install -s -o bin -g bin -m 755 vpq ${DESTDIR}/usr/lib/vpq
	install -s -o bin -g bin -m 755 vpr ${DESTDIR}/usr/lib/vpr
	install -s -o bin -g bin -m 755 vprm ${DESTDIR}/usr/lib/vprm
	install -c -o bin -g bin -m 755 vprint.sh ${DESTDIR}/usr/old/vprint
	install -c -o bin -g bin -m 755 vtroff.sh ${DESTDIR}/usr/old/vtroff
	install -c -o bin -g bin -m 755 vpq.sh ${DESTDIR}/usr/old/vpq
	install -c -o bin -g bin -m 755 vpr.sh ${DESTDIR}/usr/old/vpr
	install -c -o bin -g bin -m 755 vprm.sh ${DESTDIR}/usr/old/vprm
	install -c -o bin -g bin -m 755 tmac.vcat ${DESTDIR}/usr/share/tmac/

lint: FRC
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} lint); done
	lint ${CFLAGS} ${SRCS}

tags: FRC
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} tags); done
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

vpac: vpac.c /usr/include/stdio.h
vpd: vpd.c /usr/include/stdio.h /usr/include/sys/param.h
vpd: /usr/include/sys/types.h /usr/include/signal.h /usr/include/machine/trap.h
vpd: /usr/include/machine/machparam.h /usr/include/sys/dir.h
vpd: /usr/include/signal.h /usr/include/sys/stat.h /usr/include/sgtty.h
vpd: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
vpd: /usr/include/sys/ttydev.h /usr/include/errno.h /usr/include/sys/vcmd.h
vpd: /usr/include/sys/wait.h
vpq: vpq.c /usr/include/sys/param.h /usr/include/sys/types.h
vpq: /usr/include/signal.h /usr/include/machine/trap.h
vpq: /usr/include/machine/machparam.h /usr/include/sys/dir.h
vpq: /usr/include/sys/stat.h /usr/include/stdio.h /usr/include/errno.h
vpr: vpr.c /usr/include/signal.h /usr/include/machine/trap.h
vpr: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
vpr: /usr/include/machine/machparam.h
vprm: vprm.c /usr/include/sys/param.h /usr/include/sys/types.h
vprm: /usr/include/signal.h /usr/include/machine/trap.h
vprm: /usr/include/machine/machparam.h /usr/include/sys/dir.h
vprm: /usr/include/sys/stat.h /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
