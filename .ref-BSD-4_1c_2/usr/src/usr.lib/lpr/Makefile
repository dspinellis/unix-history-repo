#	@(#)Makefile	4.7	(Berkeley)	3/9/83
#
# makefile for line printer spooling system
#
# Ownerships {see note at the top of main in lpd.c}
# ROOT		a user that can access any and all files
# DAEMON	someone special
# SPGRP		the group id of the spooling programs
#

CFLAGS=-O -DBSD41C
LDFLAGS=
LIBDIR=/usr/lib
BINDIR=/usr/ucb
SPOOLDIR=/usr/spool/lpd
CXREF=/usr/ucb/ctags -x
ROOT=root
DAEMON=daemon
SPGRP=daemon
DESTDIR=

SRCS=	lpd.c rlpr.c lpr.c lpq.c lprm.c lpdrestart.c \
	lpdchar.c getq.c printcap.c lpf.c necf.c \
	rvsort.c rvcat.c vsort.c vcat.c vpf.c vpsf.c \
	vpltdmp.c vfontinfo.c vwidth.c pac.c
OBJS=	lpd rlpr lpr lpq lprm lpdrestart vfontinfo vwidth pac
FLTRS=	lpf necf rvsort rvcat vsort vcat vpf vpsf vpltdmp

all:	${OBJS}

filters: ${FLTRS}

lpd:	lpd.o lpdchar.o printcap.o getq.o
	${CC} -o lpd ${LDFLAGS} lpd.o lpdchar.o printcap.o getq.o -ljobs

rlpr:	rlpr.o printcap.o
	${CC} -o rlpr ${LDFLAGS} rlpr.o printcap.o

lpr:	lpr.o printcap.o
	${CC} -o lpr ${LDFLAGS} lpr.o printcap.o -ljobs

lpq:	lpq.o printcap.o getq.o
	${CC} -o lpq ${LDFLAGS} lpq.o printcap.o getq.o -ltermcap

lprm:	lprm.o printcap.o
	${CC} -o lprm ${LDFLAGS} lprm.o printcap.o

lpdrestart: lpdrestart.o
	${CC} -o lpdrestart ${LDFLAGS} lpdrestart.o

lpf:	lpf.c
	${CC} -o lpf ${CFLAGS} ${LDFLAGS} lpf.c

necf:	necf.c
	${CC} -o necf ${CFLAGS} ${LDFLAGS} necf.c

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

vpltdmp: vpltdmp.c
	${CC} -o vpltdmp ${CFLAGS} vpltdmp.c

vfontinfo: vfontinfo.c
	${CC} -o vfontinfo ${CFLAGS} vfontinfo.c

vwidth: vwidth.c
	${CC} -o vwidth ${CFLAGS} vwidth.c

pac:	pac.o printcap.o
	${CC} -o pac ${CFLAGS} pac.o printcap.o

lpd.o rlpr.o lpr.o lpq.o lprm.o lpdrestart.o pac.o: lp.local.h
lpd.o lpq.o getq.o: getq.h

install: all
	install -o ${ROOT} -m 6711 lpd ${DESTDIR}${LIBDIR}/lpd
	install -o ${DAEMON} -m 6711 rlpr ${DESTDIR}${LIBDIR}/rlpr
	install -o ${ROOT} -m 6711 lpr ${DESTDIR}${BINDIR}/lpr
	install -o ${ROOT} -m 6711 lpq ${DESTDIR}${BINDIR}/lpq
	install -o ${ROOT} -m 6711 lprm ${DESTDIR}${BINDIR}/lprm
	install -s lpdrestart ${DESTDIR}/etc/lpdrestart
	chgrp ${SPGRP} ${DESTDIR}${LIBDIR}/lpd ${DESTDIR}${LIBDIR}/rlpr \
		${DESTDIR}${BINDIR}/lpr ${DESTDIR}${BINDIR}/lpq \
		${DESTDIR}${BINDIR}/lprm
	chown ${DAEMON} ${DESTDIR}${SPOOLDIR}
	chgrp ${SPGRP} ${DESTDIR}${SPOOLDIR}
	chmod 775 ${DESTDIR}${SPOOLDIR}
	install -s vfontinfo ${DESTDIR}${LIBDIR}/vfontinfo
	install -s vwidth ${DESTDIR}${LIBDIR}/vwidth
	install -s pac ${DESTDIR}/etc/pac

finstall: filters
	install -s lpf ${DESTDIR}${LIBDIR}/lpf
	install -s necf ${DESTDIR}${LIBDIR}/necf
	install -s rvsort ${DESTDIR}${LIBDIR}/rvsort
	install -s rvcat ${DESTDIR}${LIBDIR}/rvcat
	install -s vsort ${DESTDIR}${LIBDIR}/vsort
	install -s vcat ${DESTDIR}${LIBDIR}/vcat
	install -s vpf ${DESTDIR}${LIBDIR}/vpf
	install -s vpsf ${DESTDIR}${LIBDIR}/vpsf
	install -s vpltdmp ${DESTDIR}${LIBDIR}/vpltdmp

clean:
	rm -f ${OBJS} ${FLTRS} *.o

print:
	@pr makefile
	@${CXREF} *.c | pr -h XREF
	@pr *.h *.c
