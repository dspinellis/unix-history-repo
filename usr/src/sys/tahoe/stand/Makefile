#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	7.14 (Berkeley) %G%
#

RELOC=		800
MTBOOTRELOC=	e0000
BOOTRELOC=	e0000
LDT1MBSYSOPT=	-T ${MTBOOTRELOC} -x
LDTSYSOPT=	-T ${BOOTRELOC} -x
LDTOPT=		-T ${RELOC} -e _entry -x

DESTDIR=
VPATH=	../../stand
LIBS=	libsa.a ${DESTDIR}/usr/lib/libc.a
COPTS=	-O -I. -I../..
CFLAGS=	-DSTANDALONE -DCOMPAT_42 -DTAHOE -DRELOC=0x${RELOC} ${COPTS}
SUBDIR=	vdformat
ALL=	boot cat copy ls

SRCS=	boot.c cat.c conf.c copy.c dev.c getfile.c gets.c ls.c prf.c \
	srt0.c sys.c vd.c cy.c hd.c
DRIVERS=cy.o vd.o hd.o

#all: srt0.o libsa.a ${ALL} ${SUBDIR}
all: srt0.o libsa.a ${ALL}

${SUBDIR}:
	cd $@; make ${MFLAGS}

machdep.o: machdep.c ../include/mtpr.h ../tahoe/mem.h ../tahoe/SYS.h
	${CC} -E ${CFLAGS} machdep.c | ${AS}  -o machdep.o

libsa.a: conf.o dev.o getfile.o gets.o machdep.o prf.o printn.o sys.o \
	${DRIVERS}
	ar crv $@ $?
	ranlib $@

boot: boot.o bootsrt0.o ${LIBS}
	ld ${LDTSYSOPT} bootsrt0.o boot.o ${LIBS}
	nm -u a.out
	size a.out
	dd if=a.out of=boot ibs=1024 skip=1
	-rm -f a.out

boot1mb: boot.o boot1mbsrt0.o ${LIBS}
	ld ${LDT1MBSYSOPT} boot1mbsrt0.o boot.o ${LIBS}
	nm -u a.out
	size a.out
	dd if=a.out of=boot1mb ibs=1024 skip=1
	-rm -f a.out

cat: cat.o srt0.o ${LIBS}
	ld ${LDTOPT} -o $@ -s srt0.o cat.o ${LIBS}

copy: copy.o srt0.o ncy.o ${LIBS}
	ld ${LDTOPT} -o $@ srt0.o copy.o ncy.o ${LIBS}

xpformat: xpformat.o srt0.o ${LIBS}
	ld ${LDTOPT} -o $@ srt0.o xpformat.o ${LIBS}

ls: ls.o srt0.o ${LIBS}
	ld ${LDTOPT} -o $@ -s srt0.o ls.o ${LIBS}

srt0.o: srt0.c
	${CC} -E ${CFLAGS} srt0.c | ${AS} -o srt0.o

bootsrt0.o: srt0.c
	${CC} -E -DBOOTRELOC=0x${BOOTRELOC} -DREL ${CFLAGS} srt0.c | \
	    ${AS} -o bootsrt0.o

boot1mbsrt0.o: srt0.c
	${CC} -E -DBOOTRELOC=0x${MTBOOTRELOC} -DREL ${CFLAGS} srt0.c | \
	    ${AS} -o boot1mbsrt0.o

ncy.c: cy.c
	rm -f ncy.c
	ln cy.c ncy.c

ncy.o: ncy.c
	${CC} -DNOBLOCK ${CFLAGS} -c ncy.c

clean:
	rm -f ${ALL} *.o *.map *.bak a.out ncy.c libsa.a
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done

cleandir: clean
	rm -f ${MAN} tags .depend
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} cleandir); done

depend: ${SRCS}
	mkdep ${COPTS} ${SRCS}
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done

install:
	install -o bin -g bin -m 644 cat copy ls ${DESTDIR}/stand
	install -o bin -g bin -m 644 boot ${DESTDIR}/
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install) \
	done

tags: ${SRCS}
	ctags ${SRCS}
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} tags); done
