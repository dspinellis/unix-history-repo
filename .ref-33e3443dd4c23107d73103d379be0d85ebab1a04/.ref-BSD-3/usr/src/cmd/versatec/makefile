DESTDIR=
CFLAGS=-O 

all:	sidebyside vcat vfontedpr vpac vpd vpf vpq vpr vprm vsort

sidebyside: sidebyside.o
	cc -o sidebyside sidebyside.o
vcat: vcat.o
	cc -o vcat vcat.o
vfontedpr: vfontedpr.o
	cc -o vfontedpr vfontedpr.o
vpac: vpac.o
	cc -o vpac vpac.o
vpd: vpd.o
	cc -o vpd vpd.o
vpf: vpf.o
	cc -o vpf vpf.o
vpq: vpq.o
	cc -o vpq vpq.o
vpr: vpr.o
	cc -o vpr vpr.o
vprm: vprm.o
	cc -o vprm vprm.o
vsort: vsort.o
	cc -o vsort vsort.o

install: all
	install -s sidebyside ${DESTDIR}/usr/lib/sidebyside
	install -s vcat ${DESTDIR}/usr/lib/vcat
	install -s vfontedpr ${DESTDIR}/usr/lib/vfontedpr
	install -c vgrind.sh ${DESTDIR}/usr/ucb/vgrind
	install -s vpac ${DESTDIR}/usr/ucb/vpac
	install -s vpd ${DESTDIR}/usr/lib/vpd
	install -s vpf ${DESTDIR}/usr/lib/vpf
	install -s vpq ${DESTDIR}/usr/ucb/vpq
	install -s vpr ${DESTDIR}/usr/ucb/vpr
	install -c vprint.sh ${DESTDIR}/usr/ucb/vprint
	install -c tmac.vgrind ${DESTDIR}/usr/lib/tmac/tmac.vgrind
	install -s vprm ${DESTDIR}/usr/ucb/vprm
	install -s vsort ${DESTDIR}/usr/lib/vsort
	install -c vtroff.sh ${DESTDIR}/usr/ucb/vtroff

clean:
	rm -f *.o sidebyside vcat vfontedpr vgrind vpac vpd vpf vpq vpr vprm vsort
