#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.2	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	necf.c railmag.c rvcat.c rvsort.c sidebyside.c vcat.c \
	vdmp.c vpf.c vplotf.c vpltdmp.c vpsf.c vsort.c
OBJS=	necf.o railmag.o rvcat.o rvsort.o sidebyside.o vcat.o \
	vdmp.o vpf.o vplotf.o vpltdmp.o vpsf.o vsort.o
ALL=	necf railmag rvcat rvsort sidebyside vcat \
	vdmp vpf vplotf vpltdmp vpsf vsort

all: ${ALL}

necf: ${LIBC}
	${CC} -O -o $@ necf.c
railmag: ${LIBC}
	${CC} -O -o $@ railmag.c
rvcat: ${LIBC}
	${CC} -O -o $@ rvcat.c
rvsort: ${LIBC}
	${CC} -O -o $@ rvsort.c
sidebyside: ${LIBC}
	${CC} -O -o $@ sidebyside.c
vcat: ${LIBC}
	${CC} -O -o $@ vcat.c
vdmp: ${LIBC}
	${CC} -O -o $@ vdmp.c
vpf: ${LIBC}
	${CC} -O -o $@ vpf.c
vplotf: ${LIBC} chrtab.c
	${CC} -O -o $@ vplotf.c chrtab.c
vpltdmp: ${LIBC}
	${CC} -O -o $@ vpltdmp.c
vpsf: ${LIBC}
	${CC} -O -o $@ vpsf.c
vsort: ${LIBC}
	${CC} -O -o $@ vsort.c

clean: FRC
	rm -f ${OBJS} core ${ALL} chrtab.o

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 necf ${DESTDIR}/usr/lib/necf
	install -s -o bin -g bin -m 755 railmag ${DESTDIR}/usr/lib/railmag
	install -s -o bin -g bin -m 755 rvcat ${DESTDIR}/usr/lib/rvcat
	install -s -o bin -g bin -m 755 rvsort ${DESTDIR}/usr/lib/rvsort
	install -s -o bin -g bin -m 755 sidebyside ${DESTDIR}/usr/lib/sidebyside
	install -s -o bin -g bin -m 755 vcat ${DESTDIR}/usr/lib/vcat
	install -s -o bin -g bin -m 755 vdmp ${DESTDIR}/usr/lib/vdmp
	install -s -o bin -g bin -m 755 vpf ${DESTDIR}/usr/lib/vpf
	rm -f ${DESTDIR}/usr/lib/vpfW
	ln ${DESTDIR}/usr/lib/vpf ${DESTDIR}/usr/lib/vpfW
	install -s -o bin -g bin -m 755 vplotf ${DESTDIR}/usr/lib/vplotf
	install -s -o bin -g bin -m 755 vpltdmp ${DESTDIR}/usr/lib/vpltdmp
	install -s -o bin -g bin -m 755 vpsf ${DESTDIR}/usr/lib/vpsf
	install -s -o bin -g bin -m 755 vsort ${DESTDIR}/usr/lib/vsort

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

necf: necf.c /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
necf: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
railmag: railmag.c
rvcat: rvcat.c /usr/include/stdio.h /usr/include/sys/vcmd.h
rvcat: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
rvcat: /usr/include/sys/ttydev.h /usr/include/vfont.h
rvsort: rvsort.c /usr/include/stdio.h
sidebyside: sidebyside.c /usr/include/stdio.h
vcat: vcat.c /usr/include/stdio.h /usr/include/sys/vcmd.h
vcat: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
vcat: /usr/include/sys/ttydev.h /usr/include/vfont.h
vdmp: vdmp.c /usr/include/stdio.h /usr/include/sys/vcmd.h
vdmp: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
vdmp: /usr/include/sys/ttydev.h
vpf: vpf.c /usr/include/signal.h /usr/include/machine/trap.h
vpf: /usr/include/stdio.h /usr/include/sys/vcmd.h /usr/include/sys/ioctl.h
vpf: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
vplotf: vplotf.c /usr/include/stdio.h /usr/include/vfont.h
vplotf: /usr/include/sys/vcmd.h /usr/include/sys/ioctl.h
vplotf: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
vpltdmp: vpltdmp.c /usr/include/stdio.h /usr/include/sys/vcmd.h
vpltdmp: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
vpltdmp: /usr/include/sys/ttydev.h
vpsf: vpsf.c /usr/include/stdio.h /usr/include/sys/vcmd.h
vpsf: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
vpsf: /usr/include/sys/ttydev.h
vsort: vsort.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
