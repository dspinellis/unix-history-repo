all: lpd lpf lpr lpq lprm

lpd: lpd.c
	cc -O lpd.c -o lpd

lpf: lpf.c
	cc -O lpf.c -o lpf

lpq: lpq.c
	cc -O lpq.c -o lpq

lpr: lpr.c
	cc -O lpr.c -o lpr

lprm: lprm.c
	cc -O lprm.c -o lprm

install:
	install -s lpd ${DESTDIR}/usr/lib/lpd
	install -s lpf ${DESTDIR}/usr/lib/lpf
	install -s lpq ${DESTDIR}/usr/ucb/lpq
	install -s lpr ${DESTDIR}/usr/ucb/lpr
	install -s lprm ${DESTDIR}/usr/ucb/lprm

clean:
	rm -f lpd lpf lpq lpr lprm
