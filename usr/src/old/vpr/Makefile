#	@(#)Makefile	5.8	(Berkeley)	%G%
#
DESTDIR=
CFLAGS=-O 
SOURCES=sidebyside.c vcat.c vpac.c vpd.c vpf.c vfontinfo.c \
	vpq.c vpr.c vprm.c vsort.c rvcat.c rvsort.c vdmp.c
OBJS=sidebyside vcat vpac vad vpd vpf vfontinfo \
	vpq vpr vprm vsort rvcat rvsort vdmp

all:	$(OBJS)

sidebyside: sidebyside.c
	cc -O -o sidebyside sidebyside.c
vfontinfo: vfontinfo.c
	cc -O -o vfontinfo vfontinfo.c
vcat: vcat.c
	cc -O -o vcat vcat.c
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
vdmp: vdmp.c
	cc -O -o vdmp vdmp.c

install: all
	install -s sidebyside ${DESTDIR}/usr/lib/sidebyside
	install -s vcat ${DESTDIR}/usr/lib/vcat
	install -s vpac ${DESTDIR}/etc/vpac
	install -s vad ${DESTDIR}/usr/lib/vad
	install -s vpd ${DESTDIR}/usr/lib/vpd
	install -s vpf ${DESTDIR}/usr/lib/vpf
	install -s vpq ${DESTDIR}/usr/lib/vpq
	install -s vpr ${DESTDIR}/usr/lib/vpr
	install -s vprm ${DESTDIR}/usr/lib/vprm
	install -s vsort ${DESTDIR}/usr/lib/vsort
	install -s rvsort ${DESTDIR}/usr/lib/rvsort
	install -s rvcat ${DESTDIR}/usr/lib/rvcat
	install -s vdmp ${DESTDIR}/usr/lib/vdmp
	install -s vfontinfo ${DESTDIR}/usr/lib/vfontinfo

clean:
	rm -f ${OBJS} *.o

vprintall:
	csh /usr/ucb/vprint ${SOURCES}
