#	@(#)Makefile	5.4 (Berkeley) %G%

MAN=	vlp.0

all clean depend lint tags: FRC
	cd vlp.${MACHINE}; make ${MFLAGS} DESTDIR=${DESTDIR} $@

cleandir: clean
	cd vlp.${MACHINE}; make ${MFLAGS} DESTDIR=${DESTDIR} $@
	rm -f ${MAN} tags .depend

install: ${MAN}
	cd vlp.${MACHINE}; make ${MFLAGS} DESTDIR=${DESTDIR} $@
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

FRC:
