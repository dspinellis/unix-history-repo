#	@(#)Makefile	4.5	(Berkeley)	%G%
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
	lpdchar.c getq.c printcap.c lpf.c necf.c
OBJS=	lpd rlpr lpr lpq lprm lpdrestart
filters=lpf necf

all:	${OBJS}

lpd:	lpd.o lpdchar.o printcap.o getq.o
	${CC} -o lpd ${LDFLAGS} lpd.o lpdchar.o printcap.o getq.o -ljobs

rlpr:	rlpr.o printcap.o
	${CC} -o rlpr ${LDFLAGS} rlpr.o printcap.o

lpr:	lpr.o printcap.o
	${CC} -o lpr ${LDFLAGS} lpr.o -ljobs printcap.o

lpq:	lpq.o printcap.o getq.o
	${CC} -o lpq ${LDFLAGS} lpq.o -ltermcap printcap.o getq.o

lprm:	lprm.o printcap.o
	${CC} -o lprm ${LDFLAGS} lprm.o printcap.o

lpdrestart: lpdrestart.o
	${CC} -o lpdrestart ${LDFLAGS} lpdrestart.o

lpf:	lpf.c
	${CC} -o lpf ${CFLAGS} ${LDFLAGS} lpf.c

necf:	necf.c
	${CC} -o necf ${CFLAGS} ${LDFLAGS} necf.c

lpd.o rlpr.o lpr.o lpq.o lprm.o lpdrestart.o: lp.local.h
lpd.o lpq.o getq.o: getq.h

install: all
	install -o ${ROOT} -m 6711 lpd ${DESTDIR}/${LIBDIR}/lpd
	install -o ${DAEMON} -m 6711 rlpr ${DESTDIR}/${LIBDIR}/rlpr
	install -o ${ROOT} -m 6711 lpr ${DESTDIR}/${BINDIR}/lpr
	install -o ${ROOT} -m 6711 lpq ${DESTDIR}/${BINDIR}/lpq
	install -o ${ROOT} -m 6711 lprm ${DESTDIR}/${BINDIR}/lprm
	install -s lpdrestart ${DESTDIR}/etc/lpdrestart
	chgrp ${SPGRP} ${DESTDIR}/${LIBDIR}/lpd ${DESTDIR}/${LIBDIR}/rlpr \
		${DESTDIR}/${BINDIR}/lpr ${DESTDIR}/${BINDIR}/lpq \
		${DESTDIR}/${BINDIR}/lprm
	chown ${DAEMON} ${DESTDIR}/${SPOOLDIR}
	chgrp ${SPGRP} ${DESTDIR}/${SPOOLDIR}
	chmod 775 ${DESTDIR}/${SPOOLDIR}

clean:
	rm -f ${OBJS} *.o

print:
	@pr makefile
	@${CXREF} *.c | pr -h XREF
	@pr *.h *.c
