#	from: @(#)Makefile	7.9 (Berkeley) 5/8/91

DESTDIR=/
STAND=	/sys/stand
INCPATH=-I/sys/sys -I/sys -I/sys/ufs  -I${STAND}
VPATH=	${STAND}
STANDDIR= ${DESTDIR}/stand

CC=	cc -traditional
CPP=	cpp -traditional ${INCPATH} -DSTANDALONE -DAT386

RELOC=	70000
RELOC2=	70200

CFLAGS=	-DSTANDALONE -DAT386 -O ${INCPATH}

DRIVERS=cga.c fd.c kbd.c wd.c
SRCS=	boot.c fdbootblk.c prf.c \
	srt0.c wdbootblk.c ${DRIVERS} ${SASRC}

ALL= wdboot bootwd fdboot bootfd

all: ${ALL}

# startups

srt0.o: srt0.c
	${CPP} -E -DLOCORE -DRELOC=0x${RELOC} srt0.c | ${AS} -o srt0.o

wsrt0.o: srt0.c
	${CPP} -E -DLOCORE -DSMALL -DRELOC=0x${RELOC} -DREL srt0.c | \
	    ${AS} -o wsrt0.o

relsrt0.o: srt0.c
	${CPP} -E -DLOCORE -DRELOC=0x${RELOC} -DREL srt0.c | ${AS} -o relsrt0.o

# block 0 boots

wdbootblk.o: wdbootblk.c 
	${CPP} -E -DLOCORE -DRELOC=0x${RELOC} wdbootblk.c | ${AS} -o $@

fdbootblk.o: fdbootblk.c 
	${CPP} -E -DLOCORE -DRELOC=0x${RELOC} fdbootblk.c | ${AS} -o $@

# getting booted from disc

wdboot: wdbootblk.o
	ld -N -T ${RELOC} wdbootblk.o
	cp a.out wdb
	rm -f $@; strip a.out; trimhd 32 <a.out >$@; rm -f a.out; ls -l $@

bootwd:	wsrt0.o boot.o bmap.o cga.o fs.o kbd.o prf.o wd.o printf.o breadwd.o
	ld -N -T ${RELOC2} wsrt0.o boot.o bmap.o cga.o kbd.o prf.o printf.o \
		breadwd.o fs.o wd.o -lc
	size a.out
	cp a.out bwd
	rm -f $@; strip a.out; trimhd 32 <a.out >$@; rm -f a.out; ls -l $@

fdboot: fdbootblk.o
	ld -N -T ${RELOC} fdbootblk.o
	cp a.out fdb
	rm -f $@; strip a.out; trimhd 32 <a.out >$@; rm -f a.out; ls -l $@

bootfd:	wsrt0.o boot.o bmap.o cga.o fs.o kbd.o prf.o fd.o printf.o breadfd.o
	ld -N -T ${RELOC2} wsrt0.o boot.o bmap.o cga.o kbd.o prf.o printf.o \
		breadfd.o fs.o fd.o -lc
	size a.out
	cp a.out bfd
	rm -f $@; strip a.out; trimhd 32 <a.out >$@; rm -f a.out; ls -l $@

breadwd.o: breadwd.c breadxx.o
breadfd.o: breadfd.c breadxx.o

breadxx.o:
	touch breadxx.o

breadwd.c: breadxx.c
	rm -f breadwd.c
	sed -e 's/XX/wd/' -e 's/xx/wd/g'	< breadxx.c >> breadwd.c

breadfd.c: breadxx.c
	rm -f breadfd.c
	sed -e 's/XX/fd/' -e 's/xx/fd/g'	< breadxx.c >> breadfd.c

clean:
	rm -f *.o *.exe *.i sm_*.c libdrive.a
	rm -f a.out bfd bwd fdb wdb ${ALL}
	rm -f boot[a-wyz]? boot[a-wyz]?? boot[a-wyz]?.c boot[a-wyz]??.c \
		conf[a-wyz]?.c conf[a-wyz]??.c
	rm -f format core sboot bootconf.c
	cd libsa; make cleandir
	cd libsmsa; make cleandir

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${INCPATH} -DSTANDALONE ${SRCS} ${DUMMIES}

install: ${ALL}
	cp ${ALL} ${STANDDIR}
