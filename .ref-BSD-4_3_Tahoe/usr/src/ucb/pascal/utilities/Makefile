#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.6 (Berkeley) 10/26/87
#
SCCSID = @(#)Makefile 5.6 10/26/87
INSTALL = ${DESTDIR}/usr/ucb
INSTALLIB = ${DESTDIR}/usr/lib
SRCDIR=	../src
PX_HEADER = px_header
NM = nm -go
RM = rm -f
CFLAGS = -O -I${SRCDIR}

PROGS =	pc pix pmerge
FILES =	pcexterns.o how_pc how_pi how_pix how_pxp

all: pix px_header pc pc2 pc3 pmerge pcexterns.o

install: all pxref.p
	dd if=px_header of=temp conv=sync
	install -m 644 temp ${INSTALLIB}/${PX_HEADER}
	for i in ${PROGS}; do (install -s $$i ${INSTALL}/$$i ); done
	for i in ${FILES}; do (install -m 644 -c $$i ${INSTALLIB}/$$i ); done
	install -s pc3 ${INSTALLIB}/pc3
	cd pc2; make DESTDIR=${DESTDIR} install
	-pc -w pxref.p
	install -s a.out ${INSTALL}/pxref

pix: pix.c config.o
	${CC} ${CFLAGS} -o pix pix.c config.o

pmerge: pmerge.c
	${CC} ${CFLAGS} -o pmerge pmerge.c

px_header: px_header.c ${SRCDIR}/objfmt.h config.o
	${CC} ${CFLAGS} -o px_header px_header.c config.o -N
	strip px_header

pc2: /tmp
	cd pc2; make

pc3: pc3.c pc3.h ${SRCDIR}/pstab.h
	${CC} ${CFLAGS} pc3.c -o pc3

pxref: pxref.p
	pc -w pxref.p -o pxref

pc: pc.c
	${CC} ${CFLAGS} -o pc pc.c

LIBPC 	= /usr/lib/libpc.a
LIBC	= /lib/libc.a
LIBM	= /usr/lib/libm.a
LIBG	= /usr/lib/libg.a
D_LIBPC	= $(DESTDIR)$(LIBPC)
D_LIBC	= $(DESTDIR)$(LIBC)
D_LIBM	= $(DESTDIR)$(LIBM)
D_LIBG	= $(DESTDIR)$(LIBG)
LIBRARIES = $(D_LIBPC) $(D_LIBC) $(D_LIBM) $(D_LIBG)

pcexterns.o: pcexterns.awk ${LIBRARIES}
	( echo -n $(LIBPC):; ${NM} $(D_LIBPC) ) \
		| awk -f pcexterns.awk \
		| sort -t\" +1 -2 -u >pcexterns.s
	( echo -n $(LIBC):; ${NM} $(D_LIBC) ) \
		| awk -f pcexterns.awk \
		| sort -t\" +1 -2 -u >>pcexterns.s
	( echo -n $(LIBM):; ${NM} $(D_LIBM) ) \
		| awk -f pcexterns.awk \
		| sort -t\" +1 -2 -u >>pcexterns.s
	( echo -n $(LIBG):; ${NM} $(D_LIBG) ) \
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
	${RM} pix px_header pxref pmerge pc pc3 \
	pcexterns.s *.o errs opc opc.c pic pic.c picture
	cd pc2; make clean

print: picture
	@pr READ_ME Makefile picture
	@ls -l | pr
	@pr pix.c pmerge.c pxref.p pc.c pc3.h pc3.c

depend:
