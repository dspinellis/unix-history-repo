DESTDIR=

all: pix px_header pxref

install: all
	dd if=px_header of=${DESTDIR}/usr/lib/px_header conv=sync
	cp pxref ${DESTDIR}/usr/ucb/pxref
	cp pix ${DESTDIR}/usr/ucb/pix
	cp how* ${DESTDIR}/usr/lib

pix: pix.c
	cc -O -o pix pix.c

px_header: px_header.c
	cc -O -d2 -o px_header px_header.c -N
	strip px_header

pxref: pxref.p
	/usr/ucb/pi pxref.p
	mv obj pxref

clean:
	rm -f pix px_header pxref

print:
	@pr READ_ME makefile
	@ls -l | pr
	@pr pix.c pxref.p
