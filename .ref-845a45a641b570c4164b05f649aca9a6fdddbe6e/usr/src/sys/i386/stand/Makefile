#	@(#)Makefile	7.9 (Berkeley) %G%

DESTDIR=/
STAND=	../../stand
INCPATH=-I../../sys -I../../  -I${STAND}
VPATH=	${STAND}
STANDDIR= ${DESTDIR}/stand

CC=	cc -traditional
C2=	/usr/libexec/c2
CPP=	cpp -traditional ${INCPATH} -DSTANDALONE -DAT386

RELOC=	70000
RELOC2=	70200

CFLAGS=	-DSTANDALONE -DAT386 -O ${INCPATH}

DRIVERS=cga.c fd.c kbd.c wd.c
SRCS=	boot.c bootxx.c conf.c confxx.c copy.c fd.c fdbootblk.c prf.c \
	srt0.c wdbootblk.c ${DRIVERS} ${SASRC}
LIBS=	libsa/libsa.a libdrive.a ${DESTDIR}/usr/lib/libc.a
SMLIBS=	libsmsa/libsmsa.a libdrive.a ${DESTDIR}/usr/lib/libc.a

BOOTBLOCKS= wdboot bootwd fdboot bootfd
DISKUTILS= boot copy cat ls
ALL=	${BOOTBLOCKS} ${DISKUTILS}

all: ${ALL}

libsa/libsa.a::
	cd libsa; make
libsmsa/libsmsa.a::
	cd libsmsa; make

libdrive.a: conf.o prf.o ${DRIVERS:.c=.o}
	ar crv $@ $?
	ranlib $@

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

bootwd: wdbootblk.o wsrt0.o bootxx.o sm_confwd.o sm_wd.o sm_cga.o ${SMLIBS}
	ld -N -T ${RELOC2} wsrt0.o bootxx.o sm_confwd.o sm_wd.o \
	    sm_cga.o ${SMLIBS}
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

bootfd: fdbootblk.o wsrt0.o bootxx.o fd.o sm_cga.o sm_conffd.o ${SMLIBS}
	ld -N -T ${RELOC2} wsrt0.o bootxx.o fd.o sm_cga.o sm_conffd.o ${SMLIBS}
	size a.out
	cp a.out bfd
	rm -f bootfd
	strip a.out; dd if=a.out of=bootfd ibs=32 skip=1;rm -f a.out
	ls -l bootfd

sm_conffd.o: conffd.c
	rm -f sm_conffd.c
	ln -s conffd.c sm_conffd.c
	${CC} -S -DSMALL ${CFLAGS} sm_conffd.c
	${AS} sm_conffd.s -o sm_conffd.o
	rm -f sm_conffd.s sm_conffd.c

sm_confwd.o: confwd.c
	rm -f sm_confwd.c
	ln -s confwd.c sm_confwd.c
	${CC} -S -DSMALL ${CFLAGS} sm_confwd.c
	${AS} sm_confwd.s -o sm_confwd.o
	rm -f sm_confwd.s sm_confwd.c

sm_cga.o: cga.c
	rm -f sm_cga.c
	ln -s cga.c sm_cga.c
	${CC} -S -DSMALL ${CFLAGS} sm_cga.c
	${AS} sm_cga.s -o sm_cga.o
	rm -f sm_cga.s sm_cga.c

sm_wd.o: wd.c
	rm -f sm_wd.c
	ln -s wd.c sm_wd.c
	${CC} -S -DSMALL ${CFLAGS} sm_wd.c
	${AS} sm_wd.s -o sm_wd.o
	rm -f sm_wd.s sm_wd.c

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
	install -c -s boot ${DESTDIR}
	cp ${DISKUTILS} ../dist/floppy
	cp ${TAPEUTILS} ../dist/tp
	cp ${BOOTBLOCKS} ${STANDDIR}
	cp ${DISKUTILS} ${STANDDIR}
