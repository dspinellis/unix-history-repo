#	@(#)Makefile	7.17 (Berkeley) %G%

DESTDIR=
STAND=	../../stand
INCPATH=-I. -I../..
VPATH=	${STAND}

CC=	cc
AS=	as

RELOC=		800
MTBOOTRELOC=	e0000
BOOTRELOC=	e0000
LDT1MBSYSOPT=	-T ${MTBOOTRELOC} -x
LDTSYSOPT=	-T ${BOOTRELOC} -x
LDTOPT=		-T ${RELOC} -e _entry -x

DEFS=	-DSTANDALONE -DCOMPAT_42 -DTAHOE -DRELOC=0x${RELOC}
CFLAGS=	-O ${INCPATH} ${DEFS}

SUBDIR=	vdformat
ALL=	boot cat copy ls

DRIVERS=cy.c hd.c vd.c
SASRC=  bmap.c close.c dev.c getfile.c gets.c ioctl.c lseek.c open.c \
	printf.c read.c sys.c write.c
SRCS=	boot.c cat.c conf.c copy.c cy.c hd.c ls.c prf.c srt0.c \
	${SASRC} ${DRIVERS}
LIBS=	libsa.a ${DESTDIR}/usr/lib/libc.a

all: srt0.o libsa.a ${ALL} 		# ${SUBDIR}

${SUBDIR}:
	cd $@; make ${MFLAGS}

machdep.o: machdep.c ../include/mtpr.h ../tahoe/mem.h ../tahoe/SYS.h
	${CC} -E ${CFLAGS} machdep.c | ${AS}  -o machdep.o

libsa.a: conf.o machdep.o prf.o ${DRIVERS:.c=.o} ${SASRC:.c=.o}
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
