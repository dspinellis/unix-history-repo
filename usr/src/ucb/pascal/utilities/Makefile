SCCSID = @(#)Makefile 1.13 4/16/83
INSTALL = ${DESTDIR}/usr/ucb
INSTALLIB = ${DESTDIR}/usr/lib
SRCDIR=	../src
PX_HEADER = px_header
NM = nm -go
RM = rm -f
CFLAGS = -O -I${SRCDIR}

all: pix px_header pc pc2 pc3 pmerge pcexterns.o

install: all pxref.p
	install -c -m 664 px_header ${INSTALLIB}/${PX_HEADER}
	install -c -m 775 pc ${INSTALL}
	install -c -m 775 pix ${INSTALL}
	install -c -m 664 how_pi ${INSTALLIB}
	install -c -m 664 how_pc ${INSTALLIB}
	install -c -m 664 how_pix ${INSTALLIB}
	install -c -m 664 how_pxp ${INSTALLIB}
	install -c -m 664 pcexterns.o ${INSTALLIB}
	install -c -m 775 -s pc2 ${INSTALLIB}
	install -c -m 775 -s pc3 ${INSTALLIB}
	install -c -m 775 pmerge ${INSTALL}/pmerge
	-pc -w pxref.p -o ${INSTALL}/pxref

pix: pix.c config.o
	${CC} ${CFLAGS} -o pix pix.c config.o

pmerge: pmerge.c
	${CC} ${CFLAGS} -o pmerge pmerge.c

px_header: px_header.c ${SRCDIR}/objfmt.h config.o
	${CC} ${CFLAGS} -o tpx_header px_header.c config.o -N
	strip tpx_header
	dd if=tpx_header of=px_header conv=sync
	rm tpx_header

pc2: pc2.c
	${CC} ${CFLAGS} pc2.c -lpc -o tmp
	${CC} ${CFLAGS} -S pc2.c
	./tmp pc2.s npc2.s
	as -o pc2.o npc2.s
	${CC} ${CFLAGS} pc2.o -o pc2
	rm -f pc2.o npc2.s pc2.s tmp

pc3: pc3.c pc3.h ${SRCDIR}/pstab.h
	${CC} ${CFLAGS} pc3.c -o pc3

pxref: pxref.p
	pc -w pxref.p -o pxref

pc: pc.c
	${CC} ${CFLAGS} -o pc pc.c

LIBRARIES = /usr/lib/libpc.a /lib/libc.a /usr/lib/libm.a /usr/lib/libg.a

pcexterns.o: pcexterns.awk ${LIBRARIES}
	${NM} /usr/lib/libpc.a \
		| awk -f pcexterns.awk \
		| sort -t\" +1 -2 -u >pcexterns.s
	${NM} /lib/libc.a \
		| awk -f pcexterns.awk \
		| sort -t\" +1 -2 -u >>pcexterns.s
	${NM} /usr/lib/libm.a \
		| awk -f pcexterns.awk \
		| sort -t\" +1 -2 -u >>pcexterns.s
	${NM} /usr/lib/libg.a \
		| awk -f pcexterns.awk \
		| sort -t\" +1 -2 -u >>pcexterns.s
	as pcexterns.s -o pcexterns.o
	${RM} pcexterns.s

opcode.h: ${SRCDIR}/OPnames.h ${SRCDIR}/opc.c
	${RM} opcode.h
	cc ${SRCDIR}/opc.c -o opc
	opc >opcode.h
	${RM} opc

picture: ${SRCDIR}/OPnames.h ${SRCDIR}/pic.c
	${RM} picture
	cc ${SRCDIR}/pic.c -o pic
	pic >picture
	${RM} pic

clean:
	${RM} pix px_header pxref pmerge pc pc2 pc3 \
	pcexterns.s *.o errs opc opc.c pic pic.c picture

print: picture
	@pr READ_ME makefile picture
	@ls -l | pr
	@pr pix.c pmerge.c pxref.p pc.c pc2.c pc3.h pc3.c
