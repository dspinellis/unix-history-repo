# @(#)Makefile	4.7 12/31/82
ALL=	rcp rexecd rmt route trpt whois
DESTDIR=
CFLAGS=-O

all:	${ALL}

rcp:	rcp.o
	${CC} rcp.o -o rcp -ljobs

rexecd:	rexecd.o
	${CC} rexecd.o -o rexecd -ljobs

rmt:	rmt.o
	${CC} -o rmt rmt.o -ljobs

route:	route.o
	${CC} route.o -o route

trpt:	trpt.o
	${CC} trpt.o -o trpt

whois:	whois.o
	${CC} whois.o -o whois

clean:
	rm -f ${ALL} *.o *.s

install: ${ALL}
	install -s -m 4755 rcp ${DESTDIR}/usr/ucb/rcp
	install -s rexecd ${DESTDIR}/etc/rexecd
	install -s rmt ${DESTDIR}/etc/rmt
	install -s route ${DESTDIR}/etc/route
	install -s trpt ${DESTDIR}/usr/ucb/trpt
	install -s whois ${DESTDIR}/usr/ucb/whois
