SCCSID = @(#)Makefile 1.8 %G%
INSTALL = ${DESTDIR}/usr/ucb
INSTALLIB = ${DESTDIR}/usr/lib
PX_HEADER = px_header
NM = nm -go
RM = rm -f
CFLAGS = -O

all: pix px_header pc pc2 pc3 pmerge pcexterns.o

install: all pxref.p
	dd if=px_header of=${INSTALLIB}/${PX_HEADER} conv=sync
	cp pc pix ${INSTALL}
	cp how* pc2 pc3 pcexterns.o ${INSTALLIB}
	strip ${INSTALLIB}/pc2
	strip ${INSTALLIB}/pc3
	cp pmerge ${INSTALL}/pmerge
	-pc -w pxref.p -o ${INSTALL}/pxref

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
	./tmp pc2.s | as -o pc2.o
	${CC} pc2.o -o pc2
	rm -f pc2.o pc2.s tmp

pc3: pc3.c pc3.h pstab.h
	${CC} -O pc3.c -o pc3

pxref: pxref.p
	pc -w pxref.p -o pxref

pc: pc.c
	${CC} -O -o pc pc.c

LIBRARIES = /usr/lib/libpc.a /lib/libc.a /usr/lib/libm.a /usr/lib/libg.a

pcexterns.o: pcexterns.awk ${LIBRARIES}
	${NM} /usr/lib/libpc.a | awk -f pcexterns.awk >pcexterns.s
	${NM} /lib/libc.a | awk -f pcexterns.awk >>pcexterns.s
	${NM} /usr/lib/libm.a | awk -f pcexterns.awk >>pcexterns.s
	${NM} /usr/lib/libg.a | awk -f pcexterns.awk >>pcexterns.s
	as pcexterns.s -o pcexterns.o
	${RM} pcexterns.s

clean:
	${RM} pix px_header pxref pmerge pc pc2 pc3 pcexterns.s pcexterns.o

print:
	@pr READ_ME makefile
	@ls -l | pr
	@pr pix.c pmerge.c pxref.p pc.c pc2.c
