SCCSID = @(#)Makefile 1.15 %G%
INSTALL = ${DESTDIR}/usr/ucb
INSTALLIB = ${DESTDIR}/usr/lib
SRCDIR=	../src
NM = nm -go
RM = rm -f
CFLAGS = -O -I${SRCDIR}

all: pc pc2 pc3 pmerge pcexterns.o

install: all pxref.p
	cp pc ${INSTALL}
	cp how* pc2 pc3 pcexterns.o ${INSTALLIB}
	cd ${INSTALLIB}; chmod 744 how* pcexterns.o ${PX_HEADER}
	strip ${INSTALLIB}/pc2
	strip ${INSTALLIB}/pc3
	cd ${INSTALLIB}; chmod 555 pc2 pc3
	cp pmerge ${INSTALL}/pmerge
	-pc -w pxref.p -o ${INSTALL}/pxref
	cd ${INSTALL} ; chmod 555 pmerge pxref pc 

pmerge: pmerge.c
	${CC} ${CFLAGS} -o pmerge pmerge.c

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
	${RM} pxref pmerge pc pc2 pc3 \
	pcexterns.s *.o errs opc opc.c pic pic.c picture

print: picture
	@pr READ_ME makefile picture
	@ls -l | pr
	@pr pmerge.c pxref.p pc.c pc2.c pc3.h pc3.c
