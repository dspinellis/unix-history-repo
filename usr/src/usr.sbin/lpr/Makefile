#	@(#)Makefile	4.8	(Berkeley)	%G%
#
# makefile for line printer spooling system
#
# Ownerships {see note at the top of lpd.c}
# ROOT		a user that can access any and all files
# DAEMON	someone special
# SPGRP		the group id of the spooling programs
#

CFLAGS=-O
LIBDIR=/usr/lib
BINDIR=/usr/ucb
SPOOLDIR=/usr/spool/lpd
CXREF=/usr/ucb/ctags -x
ROOT=root
DAEMON=daemon
SPGRP=daemon
DESTDIR=

SRCS=	lpd.c lpr.c lpq.c lprm.c restart.c pac.c \
	startup.c printjob.c recvjob.c displayq.c rmjob.c \
	startdaemon.c common.c printcap.c lpdchar.c lpf.c necf.c \
	rvsort.c rvcat.c vsort.c vcat.c vpf.c vpsf.c vdmp.c vpltdmp.c \
	vfontinfo.c vwidth.c vfw.c fcvt.c rotate.c rotprt.c
OBJS=	lpd lpr lpq lprm restart pac
PRINTER=	lpf necf
VARIAN=	rvsort rvcat vsort vcat vpf vpsf vdmp vpltdmp vfontinfo vwidth \
	vfw fcvt rotate rotprt

all:	${OBJS}

printer: ${PRINTER}

varian:	${VARIAN}

lpd:	lpd.o startup.o printjob.o recvjob.o displayq.o rmjob.o \
	startdaemon.o lpdchar.o common.o printcap.o
	${CC} -o lpd lpd.o startup.o printjob.o recvjob.o \
		displayq.o rmjob.o startdaemon.o lpdchar.o common.o printcap.o \
		-ljobs

lpr:	lpr.o startdaemon.o common.o printcap.o
	${CC} -o lpr lpr.o startdaemon.o common.o printcap.o -ljobs

lpq:	lpq.o displayq.o common.o printcap.o
	${CC} -o lpq lpq.o displayq.o common.o printcap.o -ltermcap

lprm:	lprm.o rmjob.o startdaemon.o common.o printcap.o
	${CC} -o lprm lprm.o rmjob.o startdaemon.o common.o printcap.o

restart: restart.o startdaemon.o common.o printcap.o
	${CC} -o restart restart.o startdaemon.o common.o printcap.o

lpf:	lpf.c
	${CC} -o lpf ${CFLAGS} lpf.c

necf:	necf.c
	${CC} -o necf ${CFLAGS} necf.c

rvsort:	rvsort.c
	${CC} -o rvsort ${CFLAGS} rvsort.c

rvcat:	rvcat.c
	${CC} -o rvcat ${CFLAGS} rvcat.c

vsort:	vsort.c
	${CC} -o vsort ${CFLAGS} vsort.c

vcat:	vcat.c
	${CC} -o vcat ${CFLAGS} vcat.c

vpf:	vpf.c
	${CC} -o vpf ${CFLAGS} vpf.c

vpsf:	vpsf.c
	${CC} -o vpsf ${CFLAGS} vpsf.c

vdmp: vdmp.c
	${CC} -o vdmp ${CFLAGS} vdmp.c

vpltdmp: vpltdmp.c
	${CC} -o vpltdmp ${CFLAGS} vpltdmp.c

vfontinfo: vfontinfo.c
	${CC} -o vfontinfo ${CFLAGS} vfontinfo.c

vwidth: vwidth.c
	${CC} -o vwidth ${CFLAGS} vwidth.c

vfw: vfw.c
	${CC} -o vfw ${CFLAGS} vfw.c

fcvt: fcvt.c
	${CC} -o fcvt ${CFLAGS} fcvt.c

rotate: rotate.c
	${CC} -o rotate ${CFLAGS} rotate.c

rotprt: rotprt.c
	${CC} -o rotprt ${CFLAGS} rotprt.c

pac:	pac.o printcap.o
	${CC} -o pac pac.o printcap.o

lpd.o lpr.o lpq.o lprm.o restart.o pac.o: lp.h lp.local.h
recvjob.o printjob.o displayq.o rmjob.o common.o: lp.h lp.local.h

install: all
	install -s -g ${SPGRP} -m 6711 lpd ${DESTDIR}${LIBDIR}/lpd
	install -s -g ${SPGRP} -m 6711 lpr ${DESTDIR}${BINDIR}/lpr
	install -s -g ${SPGRP} -m 6711 lpq ${DESTDIR}${BINDIR}/lpq
	install -s -g ${SPGRP} -m 6711 lprm ${DESTDIR}${BINDIR}/lprm
	install -s -g ${SPGRP} -m 6711 restart ${DESTDIR}${BINDIR}/restart
	chown ${DAEMON} ${DESTDIR}${SPOOLDIR}
	chgrp ${SPGRP} ${DESTDIR}${SPOOLDIR}
	chmod 775 ${DESTDIR}${SPOOLDIR}
	install -s pac ${DESTDIR}/etc/pac

installp: filters
	install -s lpf ${DESTDIR}${LIBDIR}/lpf
	install -s necf ${DESTDIR}${LIBDIR}/necf

installv: varian
	install -s vfontinfo ${DESTDIR}${LIBDIR}/vfontinfo
	install -s vwidth ${DESTDIR}${LIBDIR}/vwidth
	install -s vfw ${DESTDIR}${LIBDIR}/vfw
	install -s fcvt ${DESTDIR}${LIBDIR}/fcvt
	install -s rotate ${DESTDIR}${LIBDIR}/rotate
	install -s rotprt ${DESTDIR}${LIBDIR}/rotprt
	install -s rvsort ${DESTDIR}${LIBDIR}/rvsort
	install -s rvcat ${DESTDIR}${LIBDIR}/rvcat
	install -s vsort ${DESTDIR}${LIBDIR}/vsort
	install -s vcat ${DESTDIR}${LIBDIR}/vcat
	install -s vpf ${DESTDIR}${LIBDIR}/vpf
	install -s vpsf ${DESTDIR}${LIBDIR}/vpsf
	install -s vdmp ${DESTDIR}${LIBDIR}/vdmp
	install -s vpltdmp ${DESTDIR}${LIBDIR}/vpltdmp

clean:
	rm -f ${OBJS} ${PRINTER} ${VARIAN} *.o

print:
	@pr makefile
	@${CXREF} *.c | pr -h XREF
	@pr *.h *.c
