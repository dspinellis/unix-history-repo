#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
LIBDIR=	/usr/lib
SRCS=	lpf.c necf.c chrtab.c rvsort.c rvcat.c vsort.c vcat.c vpf.c vpsf.c \
	vdmp.c vpltdmp.c vplotf.c vfontinfo.c vwidth.c vfw.c fcvt.c rotate.c \
	rotprt.c
OBJS=	lpf.o necf.o chrtab.o rvsort.o rvcat.o vsort.o vcat.o vpf.o vpsf.o \
	vdmp.o vpltdmp.o vplotf.o vfontinfo.o vwidth.o vfw.o fcvt.o rotate.o \
	rotprt.o
PRINT=	lpf necf
VARIAN=	rvsort rvcat vsort vcat vpf vpsf vdmp vpltdmp vplotf vfontinfo vwidth \
	vfw fcvt rotate rotprt
ALL=	${PRINT} ${VARIAN}

all: ${ALL}

lpf: lpf.c
	${CC} -o $@ ${CFLAGS} lpf.c

necf: necf.c
	${CC} -o $@ ${CFLAGS} necf.c

rvsort: rvsort.c
	${CC} -o $@ ${CFLAGS} rvsort.c

rvcat: rvcat.c
	${CC} -o $@ ${CFLAGS} rvcat.c

vsort: vsort.c
	${CC} -o $@ ${CFLAGS} vsort.c

vcat: vcat.c
	${CC} -o $@ ${CFLAGS} vcat.c

vpf: vpf.c
	${CC} -o $@ ${CFLAGS} vpf.c

vpsf: vpsf.c
	${CC} -o $@ ${CFLAGS} vpsf.c

vdmp: vdmp.c
	${CC} -o $@ ${CFLAGS} vdmp.c

vpltdmp: vpltdmp.c
	${CC} -o $@ ${CFLAGS} vpltdmp.c

vplotf: vplotf.c chrtab.o
	${CC} -o $@ ${CFLAGS} vplotf.c chrtab.o

vfontinfo: vfontinfo.c
	${CC} -o $@ ${CFLAGS} vfontinfo.c

vwidth: vwidth.c
	${CC} -o $@ ${CFLAGS} vwidth.c

vfw: vfw.c
	${CC} -o $@ ${CFLAGS} vfw.c

fcvt: fcvt.c
	${CC} -o $@ ${CFLAGS} fcvt.c

rotate: rotate.c
	${CC} -o $@ ${CFLAGS} rotate.c

rotprt: rotprt.c
	${CC} -o $@ ${CFLAGS} rotprt.c

clean: FRC
	rm -f ${OBJS} core ${ALL}

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 lpf ${DESTDIR}/${LIBDIR}/lpf
	install -s -o bin -g bin -m 755 necf ${DESTDIR}/${LIBDIR}/necf

installv:
	install -s -o bin -g bin -m 755 vfontinfo ${DESTDIR}/${LIBDIR}/vfontinfo
	install -s -o bin -g bin -m 755 vwidth ${DESTDIR}/${LIBDIR}/vwidth
	install -s -o bin -g bin -m 755 vfw ${DESTDIR}/${LIBDIR}/vfw
	install -s -o bin -g bin -m 755 fcvt ${DESTDIR}/${LIBDIR}/fcvt
	install -s -o bin -g bin -m 755 rotate ${DESTDIR}/${LIBDIR}/rotate
	install -s -o bin -g bin -m 755 rotprt ${DESTDIR}/${LIBDIR}/rotprt
	install -s -o bin -g bin -m 755 rvsort ${DESTDIR}/${LIBDIR}/rvsort
	install -s -o bin -g bin -m 755 rvcat ${DESTDIR}/${LIBDIR}/rvcat
	install -s -o bin -g bin -m 755 vsort ${DESTDIR}/${LIBDIR}/vsort
	install -s -o bin -g bin -m 755 vcat ${DESTDIR}/${LIBDIR}/vcat
	install -s -o bin -g bin -m 755 vpf ${DESTDIR}/${LIBDIR}/vpf
	ln ${DESTDIR}/${LIBDIR}/vpf ${DESTDIR}/${LIBDIR}/vpfW
	install -s -o bin -g bin -m 755 vpsf ${DESTDIR}/${LIBDIR}/vpsf
	install -s -o bin -g bin -m 755 vdmp ${DESTDIR}/${LIBDIR}/vdmp
	install -s -o bin -g bin -m 755 vpltdmp ${DESTDIR}/${LIBDIR}/vpltdmp
	install -s -o bin -g bin -m 755 vplotf ${DESTDIR}/${LIBDIR}/vplotf

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- make depend uses it

