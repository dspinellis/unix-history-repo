all:
	cc -O lpd.c -o lpd
	cc -O lpf.c -o lpf
	cc -O lpq.c -o lpq
	cc -O lpr.c -o lpr
	cc -O lprm.c -o lprm
install:
	install -s lpd ${DESTDIR}/usr/lib/lpd
	install -s lpf ${DESTDIR}/usr/lib/lpf
	install -s lpq ${DESTDIR}/usr/ucb/lpq
	install -s lpr ${DESTDIR}/usr/ucb/lpr
	install -s lprm ${DESTDIR}/usr/ucb/lprm
clean:
	rm -f lpd lpf lpq lpr lprm
