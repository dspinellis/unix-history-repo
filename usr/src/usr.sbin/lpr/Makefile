#
# makefile for line printer spooling system
#
# Ownerships {see note at the top of main in lpd.c}
# ROOT		a user that can access any and all files
# DAEMON	someone special
# SPGRP		the group id of the spooling programs
#
CFLAGS=-O
LDFLAGS=
DESTDIR=
LIBDIR=${DESTDIR}/usr/lib
BINDIR=${DESTDIR}/usr/ucb
SPOOLDIR=${DESTDIR}/usr/spool/lpd
CXREF=/usr/ucb/ctags -x
ROOT=root
DAEMON=daemon
SPGRP=daemon

all: lpd lpr lpq lprm lpdrestart filters

filters: necf lpf laf

lpd:	lpd.o lpdchar.o printcap.o getqueue.o
	${CC} -o lpd ${LDFLAGS} lpd.o lpdchar.o printcap.o getqueue.o

lpr:	lpr.o printcap.o
	${CC} -o lpr ${LDFLAGS} lpr.o printcap.o

lpq:	lpq.o printcap.o getqueue.o
	${CC} -o lpq ${LDFLAGS} lpq.o -ltermcap printcap.o getqueue.o

lprm:	lprm.o printcap.o
	${CC} -o lprm ${LDFLAGS} lprm.o printcap.o

lpdrestart: lpdrestart.o
	${CC} -o lpdrestart ${LDFLAGS} lpdrestart.o

laf:	laf.c
	${CC} -o laf ${CFLAGS} ${LDFLAGS} laf.c

lpf:	lpf.c
	${CC} -o lpf ${CFLAGS} ${LDFLAGS} lpf.c

necf:	necf.c
	${CC} -o necf ${CFLAGS} ${LDFLAGS} necf.c

lpr.o lpd.o lpq.o lprm.o lpdrestart.o: lp.local.h
lpd.o lpq.o getqueue.o: getqueue.h

install: all
	cp lpd ${LIBDIR}/lpd
	cp lpq ${BINDIR}/lpq
	cp lprm ${BINDIR}/lprm
	cp lpr ${BINDIR}/lpr
#	rm -f ${BINDIR}/opr ${BINDIR}/print
#	ln ${BINDIR}/lpr ${BINDIR}/print
#	ln ${BINDIR}/lpr ${BINDIR}/opr
	chown ${ROOT} ${BINDIR}/lpr
	chown ${DAEMON} $(BINDIR)/lpq ${BINDIR}/lprm ${LIBDIR}/lpd
	chgrp ${SPGRP} ${LIBDIR}/lpd ${BINDIR}/lpr ${BINDIR}/lpq ${BINDIR}/lprm
	chmod 6711 ${LIBDIR}/lpd ${BINDIR}/lpr ${BINDIR}/lprm
	chmod 2711 ${BINDIR}/lpq
	chown ${DAEMON} ${SPOOLDIR}
	chgrp ${SPGRP} ${SPOOLDIR}
	chmod 775 ${SPOOLDIR}

clean:
	rm -f laf lprm lpq lpd lpr necf lpf *.o

print:
	@pr makefile
	@${CXREF} *.c | pr -h XREF
	@pr *.h *.c

