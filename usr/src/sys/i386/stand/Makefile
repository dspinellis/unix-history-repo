#	@(#)Makefile	7.4 (Berkeley) %G%

DESTDIR=/
STAND=	../../stand
INCPATH=-I../../sys -I../../  -I${STAND}
VPATH=	${STAND}
CC=	cc -traditional
C2=	/usr/libexec/c2
CPP=	cpp -traditional ${INCPATH} -DSTANDALONE -DAT386
CFLAGS=	-DSTANDALONE -DAT386 -O ${INCPATH}
RELOC=	70000
RELOC2=	70200
SRCS=	boot.c bootxx.c cga.c conf.c confxx.c copy.c dev.c fd.c fdbootblk.c \
	getfile.c gets.c kbd.c prf.c printf.c srt0.c sys.c wd.c wdbootblk.c
DRIVERS=kbd.o cga.o wd.o fd.o
LIBS=	libsa.a ${DESTDIR}/usr/lib/libc.a

BOOTBLOCKS= wdboot bootwd fdboot bootfd
DISKUTILS= boot copy cat ls
ALL=	${BOOTBLOCKS} ${DISKUTILS}

STANDDIR= ${DESTDIR}/stand

all: ${ALL}

libsa.a: conf.o dev.o getfile.o gets.o prf.o printf.o sys.o \
	${DRIVERS}
	ar crv $@ $?
	ranlib $@

${DRIVERS}:
	${CC} -c ${CFLAGS} $*.c

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

# bootable from floppy or real disks

boot:	boot.o bootconf.o cga.o relsrt0.o ${LIBS}
	ld -N -T ${RELOC} -o $@ boot.o bootconf.o cga.o relsrt0.o ${LIBS}

bootconf.o: conf.o
	ln -s conf.c bootconf.c
	${CC} -c ${CFLAGS} -DBOOT bootconf.c
	rm -f bootconf.c

# utilities

cat:	cat.o srt0.o conf.o ${LIBS}
	ld -N -o $@ srt0.o cat.o conf.o ${LIBS}

ls:	ls.o srt0.o conf.o ${LIBS}
	ld -N -o $@ srt0.o ls.o conf.o ${LIBS}

copy:	copy.o srt0.o conf.o ${LIBS}
	ld -N -o $@ srt0.o copy.o conf.o ${LIBS}

# getting booted from disc

wdboot: wdbootblk.o
	ld -N -T ${RELOC} wdbootblk.o
	cp a.out wdb
	rm -f wdboot
	strip a.out;dd if=a.out of=wdboot ibs=32 skip=1;rm -f a.out
	ls -l wdboot

bootwd: wdbootblk.o wsrt0.o bootxx.o confwd.o sm_wd.o sys.o sm_cga.o ${LIBS}
	ld -N -T ${RELOC2} wsrt0.o bootxx.o confwd.o sys.o sm_wd.o \
	    sm_cga.o ${LIBS}
	size a.out
	cp a.out bwd
	rm -f bootwd
	strip a.out;dd if=a.out of=bootwd ibs=32 skip=1;rm -f a.out
	ls -l bootwd

fdboot: fdbootblk.o
	ld -N -T ${RELOC} fdbootblk.o
	cp a.out fdb
	rm -f fdboot
	strip a.out;dd if=a.out of=fdboot ibs=32 skip=1;rm -f a.out
	ls -l fdboot

bootfd: fdbootblk.o wsrt0.o bootxx.o conffd.o fd.o sys.o sm_cga.o ${LIBS}
	ld -N -T ${RELOC2} wsrt0.o bootxx.o conffd.o sys.o \
		fd.o sm_cga.o ${LIBS}
	size a.out
	cp a.out bfd
	rm -f bootfd
	strip a.out; dd if=a.out of=bootfd ibs=32 skip=1;rm -f a.out
	ls -l bootfd

sm_wd.o: wd.o
	rm -f sm_wd.c
	ln -s wd.c sm_wd.c
	${CC} -S -DSMALL ${CFLAGS} sm_wd.c
	${AS} sm_wd.s -o sm_wd.o
	rm -f sm_wd.s sm_wd.c

sm_cga.o: cga.o
	rm -f sm_cga.c
	ln -s cga.c sm_cga.c
	${CC} -S -DSMALL ${CFLAGS} sm_cga.c
	${AS} sm_cga.s -o sm_cga.o
	rm -f sm_cga.s sm_cga.c

sm_cst.o: cst.o
	rm -f sm_cst.c
	ln -s cst.c sm_cst.c
	${CC} -S -DSMALL ${CFLAGS} sm_cst.c
	${C2} -i sm_cst.s | ${AS} -o sm_cst.o
	rm -f sm_cst.s sm_cst.c

sm_swd.o: swd.o
	rm -f sm_swd.c
	ln -s swd.c sm_swd.c
	${CC} -S -DSMALL ${CFLAGS} sm_swd.c
	${C2} -i sm_swd.s | ${AS} -o sm_swd.o
	rm -f sm_swd.s sm_swd.c

sys.o: sys.c
	${CC} -c -DSMALL ${CFLAGS} ${.ALLSRC}

confwd.o: confwd.c confxx.o
conffd.o: conffd.c confxx.o

confxx.o:
	touch confxx.o

confwd.c: confxx.c
	rm -f confwd.c
	sed -e 's/io->i_dev/0/g'		< ${STAND}/dev.c > confwd.c
	sed -e 's/XX/wd/' -e 's/xx/wd/g'	< confxx.c >> confwd.c

conffd.c: confxx.c
	rm -f conffd.c
	sed -e 's/io->i_dev/0/g'		< ${STAND}/dev.c > conffd.c
	sed -e 's/XX/fd/' -e 's/xx/fd/g'	< confxx.c >> conffd.c

clean:
	rm -f *.o *.exe *.i sm_*.c libsa.a
	rm -f a.out bfd bwd fdb wdb ${ALL}
	rm -f boot[a-wyz]? boot[a-wyz]?? boot[a-wyz]?.c boot[a-wyz]??.c \
		conf[a-wyz]?.c conf[a-wyz]??.c
	rm -f format core sboot bootconf.c

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${INCPATH} -DSTANDALONE ${SRCS} ${DUMMIES}

install: ${ALL}
	install -c -s boot ${DESTDIR}
	cp ${DISKUTILS} ../dist/floppy
	cp ${TAPEUTILS} ../dist/tp
	cp ${BOOTBLOCKS} ${STANDDIR}
	cp ${DISKUTILS} ${STANDDIR}
