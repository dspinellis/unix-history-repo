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

all: lpd lpr sq dq lpdrestart filters

filters: necf lpf laf

lpd:	lpd.o lpdchar.o printcap.o
	${CC} -o lpd ${LDFLAGS} lpd.o lpdchar.o printcap.o

lpr:	lpr.o printcap.o
	${CC} -o lpr ${LDFLAGS} lpr.o printcap.o

sq:	sq.o printcap.o
	${CC} -o sq ${LDFLAGS} sq.o -ltermcap printcap.o

dq:	dq.o printcap.o
	${CC} -o dq ${LDFLAGS} dq.o printcap.o

lpdrestart: lpdrestart.o
	${CC} -o lpdrestart ${LDFLAGS} lpdrestart.o

laf:	laf.c
	${CC} -o laf ${CFLAGS} ${LDFLAGS} laf.c

lpf:	lpf.c
	${CC} -o lpf ${CFLAGS} ${LDFLAGS} lpf.c

necf:	necf.c
	${CC} -o necf ${CFLAGS} ${LDFLAGS} necf.c

lpr.o lpd.o sq.o dq.o lpdrestart.o: lp.local.h

install: all
	cp lpd ${LIBDIR}/lpd
	cp sq ${BINDIR}/sq
	cp dq ${BINDIR}/dq
	cp lpr ${BINDIR}/lpr
#	rm -f ${BINDIR}/opr ${BINDIR}/print
#	ln ${BINDIR}/lpr ${BINDIR}/print
#	ln ${BINDIR}/lpr ${BINDIR}/opr
	chown ${ROOT} ${BINDIR}/lpr ${BINDIR}/sq
	chown ${DAEMON} ${BINDIR}/dq ${LIBDIR}/lpd
	chgrp ${SPGRP} ${LIBDIR}/lpd ${BINDIR}/lpr ${BINDIR}/dq ${BINDIR}/sq
	chmod 6711 ${LIBDIR}/lpd ${BINDIR}/lpr ${BINDIR}/dq
	chmod 2711 ${BINDIR}/sq
	chmod 775 ${SPOOLDIR}
	chgrp ${SPGRP} ${SPOOLDIR}
	chown ${DAEMON} ${SPOOLDIR}

clean:
	rm -f laf dq sq lpd lpr necf lpf *.o

print:
	@pr makefile
	@${CXREF} *.c | pr -h XREF
	@pr *.h *.c

