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
LIBDIR=/usr/lib
BINDIR=/usr/ucb
SPOOLDIR=/usr/spool/lpd
CXREF=/usr/ucb/ctags -x
ROOT=root
DAEMON=daemon
SPGRP=daemon

all: lpd rlpr lpr lpq lprm lpdrestart filters

filters: laf lpf necf

lpd:	lpd.o lpdchar.o printcap.o getq.o
	${CC} -o lpd ${LDFLAGS} lpd.o lpdchar.o printcap.o getq.o

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

laf:	laf.c
	${CC} -o laf ${CFLAGS} ${LDFLAGS} laf.c

lpf:	lpf.c
	${CC} -o lpf ${CFLAGS} ${LDFLAGS} lpf.c

necf:	necf.c
	${CC} -o necf ${CFLAGS} ${LDFLAGS} necf.c

lpd.o rlpr.o lpr.o lpq.o lprm.o lpdrestart.o: lp.local.h
lpd.o lpq.o getq.o: getq.h

install: lpd rlpr lpr lpq lprm
	install -o ${ROOT} -m 6711 lpd ${LIBDIR}/lpd
	install -o ${DAEMON} -m 6711 rlpr ${LIBDIR}/rlpr
	install -o ${ROOT} -m 6711 lpr ${BINDIR}/lpr
	install -o ${ROOT} -m 6711 lpq ${BINDIR}/lpq
	install -o ${ROOT} -m 6711 lprm ${BINDIR}/lprm
	chgrp ${SPGRP} ${LIBDIR}/lpd ${LIBDIR}/rlpr ${BINDIR}/lpr \
		${BINDIR}/lpq ${BINDIR}/lprm
	chown ${DAEMON} ${SPOOLDIR}
	chgrp ${SPGRP} ${SPOOLDIR}
	chmod 775 ${SPOOLDIR}

clean:
	rm -f lpd rlpr lpr lpq lprm lpdrestart laf lpf necf *.o

print:
	@pr makefile
	@${CXREF} *.c | pr -h XREF
	@pr *.h *.c

