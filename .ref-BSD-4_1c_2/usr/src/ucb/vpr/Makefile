#
#	@(#)Makefile	4.1	(Berkeley)	3/8/83
#
DESTDIR=

SOURCES=vprint.sh vtroff.sh vpq.sh vpr.sh vprm.sh

all:	${SOURCES}

install: all
	install -c vprint.sh ${DESTDIR}/usr/ucb/vprint
	install -c vtroff.sh ${DESTDIR}/usr/ucb/vtroff
	install -c vpq.sh ${DESTDIR}/usr/ucb/vpq
	install -c vpr.sh ${DESTDIR}/usr/ucb/vpr
	install -c vprm.sh ${DESTDIR}/usr/ucb/vprm

clean:
	rm -f errs

vprintall:
	csh /usr/ucb/vprint ${SOURCES}
