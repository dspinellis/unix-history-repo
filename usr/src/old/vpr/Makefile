DESTDIR=
CFLAGS=-O 
SOURCES=sidebyside.c vcat.c vfontedpr.c vgrindefs.c regexp.c vpac.c vpd.c vpf.c vpq.c vpr.c vprm.c vsort.c
OBJS=sidebyside vcat vfontedpr vpac vad vpd vpf vpq vpr vprm vsort rvcat rvsort

all:	$(OBJS)

sidebyside: sidebyside.c
	cc -O -o sidebyside sidebyside.c
vcat: vcat.c
	cc -O -o vcat vcat.c
vfontedpr: vfontedpr.c vgrindefs.o regexp.o
	cc -O -o vfontedpr vfontedpr.c vgrindefs.o regexp.o
retest: retest.c regexp.o
	cc -g -o retest retest.c regexp.o
vgrindefs.o: vgrindefs.c
	cc -O -c vgrindefs.c
regexp.o: regexp.c
	cc -O -c regexp.c
vpac: vpac.c
	cc -O -o vpac vpac.c
vad: vpd.c
	cc -DVARIAN -O -o vad vpd.c
vpd: vpd.c
	cc -DVERSATEC -O -o vpd vpd.c
vpf: vpf.c
	cc -O -o vpf vpf.c
vpq: vpq.c
	cc -O -o vpq vpq.c
vpr: vpr.c
	cc -O -o vpr vpr.c
vprm: vprm.c
	cc -O -o vprm vprm.c
vsort: vsort.c
	cc -O -o vsort vsort.c
rvsort: rvsort.c
	cc -O -o rvsort rvsort.c
rvcat: rvcat.c
	cc -O -o rvcat rvcat.c

install: all
	install -s sidebyside ${DESTDIR}/usr/lib/sidebyside
	install -s vcat ${DESTDIR}/usr/lib/vcat
	install -s vfontedpr ${DESTDIR}/usr/lib/vfontedpr
	install -c vgrind.sh ${DESTDIR}/usr/ucb/vgrind
	install -c vgrindefs.src ${DESTDIR}/usr/lib/vgrindefs
	install -s vpac ${DESTDIR}/etc
	install -s vad ${DESTDIR}/usr/lib/vad
	install -s vpd ${DESTDIR}/usr/lib/vpd
	install -s vpf ${DESTDIR}/usr/lib/vpf
	install -s vpq ${DESTDIR}/usr/ucb/vpq
	install -s vpr ${DESTDIR}/usr/ucb/vpr
	install -c vprint.sh ${DESTDIR}/usr/ucb/vprint
	install -c tmac.vgrind ${DESTDIR}/usr/lib/tmac/tmac.vgrind
	install -s vprm ${DESTDIR}/usr/ucb/vprm
	install -s vsort ${DESTDIR}/usr/lib/vsort
	install -c vtroff.sh ${DESTDIR}/usr/ucb/vtroff
	install -s rvsort ${DESTDIR}/usr/lib/rvsort
	install -s rvcat ${DESTDIR}/usr/lib/rvcat

clean:
	rm -f ${OBJS}

vprintall:
	csh /usr/ucb/vprint ${SOURCES}
