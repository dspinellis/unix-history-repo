SCCSID = @(#)makefile 1.2 4/21/81
INSTALL = ${DESTDIR}/usr/ucb
INSTALLIB = ${DESTDIR}/usr/lib
PX_HEADER = px_header
CFLAGS = -O

all: pix px_header pc pc2 pc3 pmerge

install: all pxref.p
	dd if=px_header of=${INSTALLIB}/${PX_HEADER} conv=sync
	cp pc pix ${INSTALL}
	cp how* pc2 pc3 ${INSTALLIB}
	cp pmerge ${INSTALL}/pmerge
	pc -w pxref.p -o ${INSTALL}/pxref

pix: pix.c
	${CC} -O -o pix pix.c

pmerge: pmerge.c
	${CC} -O -o pmerge pmerge.c

px_header: px_header.c objfmt.h
	${CC} -O -d2 -o px_header px_header.c -N
	strip px_header

pc2: pc2.c
	${CC} pc2.c -lpc -o tmp
	${CC} -S pc2.c
	./tmp pc2.s | as -d2 -o pc2.o
	${CC} pc2.o -o pc2 -s
	rm -f pc2.o pc2.s tmp

pc3: pc3.c pc3.h pstab.h
	${CC} -O -d2 pc3.c -o pc3 -s

pxref: pxref.p
	pc -w pxref.p -o pxref

pc: pc.c
	${CC} -O -d2 -o pc pc.c

clean:
	rm -f pix px_header pxref pmerge pc pc2 pc3

print:
	@pr READ_ME makefile
	@ls -l | pr
	@pr pix.c pmerge.c pxref.p pc.c pc2.c
