#	@(#)Makefile	4.10 (Berkeley) %G%

MAN=	doctor.0

all: doctor

doctor:
	liszt -mrq -o doctor doctor.l

install: ${MAN}
	install -o games -g bin -m 700 doctor ${DESTDIR}/usr/games/hide
	rm -f ${DESTDIR}/usr/games/doctor
	ln -s ${DESTDIR}/usr/games/dm ${DESTDIR}/usr/games/doctor
	chown games.bin ${DESTDIR}/usr/games/doctor
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6

clean:
	rm -f doctor

cleandir: clean
	rm -f ${MAN}

depend lint tags:
