#	@(#)Makefile	5.6 (Berkeley) %G%

MAN=	fp.0

all clean depend lint tags: FRC
	cd fp.${MACHINE}; make ${MFLAGS} DESTDIR=${DESTDIR} $@

cleandir: clean
	cd fp.${MACHINE}; make ${MFLAGS} DESTDIR=${DESTDIR} $@
	rm -f ${MAN} tags .depend

install: ${MAN}
	cd fp.${MACHINE}; make ${MFLAGS} DESTDIR=${DESTDIR} $@
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

FRC:
