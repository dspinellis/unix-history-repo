#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh
#
#	@(#)Makefile	7.24 (Berkeley) %G%
#

DESTDIR=
STAND=	../../stand
INCPATH=-I. -I../..
VPATH=	${STAND}:../vax
C2=	/usr/libexec/c2
DEFS=	${INCPATH} -DSTANDALONE -DCOMPAT_42
MACH=	-DVAX8600 -DVAX8200 -DVAX780 -DVAX750 -DVAX730 -DVAX630 -DVAX650
LS=	/bin/ls

# Collections of C options:
#	COPTS	device drivers/assembler, not optimized
#	SMOPTS	"small" versions for 7.5K bootxx programs
#	730OPTS	"small" versions for 11/730-only versions
#	CFLAGS	everything else

COPTS=	${DEFS} ${MACH}
CFLAGS=	-O ${DEFS} ${MACH}
730OPTS=-O ${DEFS} -DVAX730
SMOPTS=	${DEFS} -DSMALL -DVAX750 -DVAX630 -DVAX650

RELOC=	150000
LIBSA=	libsa.a

SRCS=	autoconf.c boot.c bootxx.c cat.c copy.c dev.c drtest.c format.c \
	format.c getfile.c gets.c hp.c hpmaptype.c ht.c idc.c kdb.c \
	machdep.c mba.c mt.c prf.c printn.c qdcons.c qvcons.c qfont.c \
	rk.c rl.c sys.c tm.c tmscp.c ts.c uba.c uda.c up.c upmaptype.c \
	ut.c conf.c ../vax/dkbad.c
DUMMIES=confxx.c
DRIVERS=autoconf.o hp.o hpmaptype.o ht.o idc.o kdb.o mba.o mt.o qdcons.o \
	qvcons.o rk.o rl.o tm.o tmscp.o ts.o up.o upmaptype.o uba.o uda.o \
	ut.o
SMOBJ=	bootxx.o sm_sys.o sm_autoconf.o 

# These drivers don't have ecc correction and bad sector forwarding;
# they are placed in the file system boot area for 750's.  If your
# root has bad sectors you can try and squeeze the newer drivers in...
ODRIVERS=sm_hp.o sm_up.o

MDEC=	boothp boothk bootkra bootup bootra bootrl
ALL=	boot tpboot cat copy tpcopy format tpformat drtest ls \
	730boot 730copy 730format 730drtest ${MDEC}

all: ${ALL}

${LIBSA}: conf.o dev.o dkbad.o getfile.o gets.o machdep.o prf.o printn.o \
	qfont.o sys.o ${DRIVERS}
	ar crv ${LIBSA} $?
	ranlib ${LIBSA}

${DRIVERS}:
	${CC} -c -S ${COPTS} $*.c
	${C2} -i $*.s | as -o $*.o
	rm -f $*.s

# startups

srt0.o: srt0.c ../include/mtpr.h ../include/cpu.h
	${CC} -E -DRELOC=0x${RELOC} ${COPTS} srt0.c | as -o srt0.o

tpsrt0.o: srt0.c ../include/mtpr.h ../include/cpu.h
	${CC} -E -DRELOC=0x${RELOC} -DTP ${COPTS} srt0.c | as -o tpsrt0.o

relsrt0.o: srt0.c ../include/mtpr.h ../include/cpu.h
	${CC} -E -DRELOC=0x${RELOC} -DREL ${COPTS} srt0.c | as -o relsrt0.o

# bootable from tape

tpboot: tpboot.o relsrt0.o ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o tpboot.o ${LIBSA} -lc
	strip a.out; dd if=a.out of=tpboot ibs=32 skip=1; rm -f a.out

tpboot.o: boot.o
	rm -f $*.c
	ln -s boot.c $*.c
	${CC} -c ${CFLAGS} -DJUSTASK $*.c
	rm -f $*.c

tpcopy: copy.o tpsrt0.o ${LIBSA}
	ld -N tpsrt0.o copy.o ${LIBSA} -lc
	strip a.out; dd if=a.out of=tpcopy ibs=32 skip=1; rm -f a.out

tpformat: tpformat.o tpsrt0.o confhpup.o ${LIBSA}
	ld -N tpsrt0.o tpformat.o confhpup.o ${LIBSA} -lc
	strip a.out; dd if=a.out of=tpformat ibs=32 skip=1; rm -f a.out

tpformat.o: format.o
	rm -f $*.c
	ln -s format.c $*.c
	${CC} -c ${CFLAGS} -DJUSTEXIT $*.c
	rm -f $*.c

# bootable from floppy or real disks

boot: boot.o relsrt0.o bootconf.o ${LIBSA}
	ld -N -T ${RELOC} -o $@ relsrt0.o boot.o bootconf.o ${LIBSA} -lc

bootconf.o: conf.o
	rm -f $*.c
	ln -s conf.c $*.c
	${CC} -c ${CFLAGS} -DBOOT $*.c
	rm -f $*.c

copy: copy.o srt0.o getfile.o ${LIBSA}
	ld -N -o $@ srt0.o copy.o getfile.o ${LIBSA} -lc

cat: cat.o srt0.o getfile.o ${LIBSA}
	ld -N -o $@ srt0.o cat.o getfile.o ${LIBSA} -lc

ls: ls.o srt0.o ${LIBSA}
	ld -N -o $@ srt0.o ls.o ${LIBSA} -lc

format: format.o srt0.o confhpup.o ${LIBSA}
	ld -N -o $@ srt0.o format.o confhpup.o ${LIBSA} -lc

drtest: drtest.o srt0.o confhpup.o ${LIBSA}
	ld -N -o $@ srt0.o drtest.o confhpup.o ${LIBSA} -lc

# for 730s minimize size to avoid microcode botch
# (won't load files larger than 12.5 Kbytes)

730boot.o: boot.o
	rm -f $*.c
	ln -s boot.c $*.c
	${CC} -c ${730OPTS} $*.c
	rm -f $*.c

730boot: 730boot.o relsrt0.o 730bootconf.o ${LIBSA}
	ld -N -T ${RELOC} -o 730boot relsrt0.o 730boot.o 730bootconf.o ${LIBSA} -lc

730bootconf.o: conf.o
	rm -f $*.c
	ln -s conf.c $*.c
	${CC} -c ${730OPTS} -DBOOT $*.c
	rm -f $*.c

730copy: copy.o srt0.o 730conf.o ${LIBSA}
	ld -N -o $@ srt0.o copy.o 730conf.o ${LIBSA} -lc

730drtest: drtest.o srt0.o confup.o ${LIBSA}
	ld -N -o $@ srt0.o drtest.o confup.o ${LIBSA} -lc

730format: format.o srt0.o confup.o ${LIBSA}
	ld -N -o $@ srt0.o format.o confup.o ${LIBSA} -lc

730conf.o: conf.o
	rm -f $*.c
	ln -s conf.c $*.c
	${CC} -c ${730OPTS} $*.c
	rm -f $*.c

# bootstrap from ether

### not yet, rosin, not yet ###

# getting booted from disc

boothk: relsrt0.o confrk.o ${SMOBJ} ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o confrk.o ${SMOBJ} ${LIBSA} -lc
	size a.out
	rm -f $@
	strip a.out;dd if=a.out of=$@ ibs=32 skip=1;rm -f a.out
	${LS} -l $@

boothp: relsrt0.o confhp.o sm_hp.o ${SMOBJ} ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o confhp.o sm_hp.o ${SMOBJ} ${LIBSA} -lc
	size a.out
	rm -f $@
	strip a.out;dd if=a.out of=$@ ibs=32 skip=1;rm -f a.out
	${LS} -l $@

bootkra: relsrt0.o confkra.o ${SMOBJ} ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o confkra.o ${SMOBJ} ${LIBSA} -lc
	size a.out
	rm -f $@
	strip a.out;dd if=a.out of=$@ ibs=32 skip=1;rm -f a.out
	${LS} -l $@

bootup: relsrt0.o confup.o sm_up.o ${SMOBJ} ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o confup.o sm_up.o ${SMOBJ} ${LIBSA} -lc
	size a.out
	rm -f $@
	strip a.out;dd if=a.out of=$@ ibs=32 skip=1;rm -f a.out
	${LS} -l $@

bootra: relsrt0.o confra.o ${SMOBJ} ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o confra.o ${SMOBJ} ${LIBSA} -lc
	size a.out
	rm -f $@
	strip a.out;dd if=a.out of=$@ ibs=32 skip=1;rm -f a.out
	${LS} -l $@

bootrl: relsrt0.o confrl.o ${SMOBJ} ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o confrl.o ${SMOBJ} ${LIBSA} -lc
	size a.out
	rm -f $@
	strip a.out;dd if=a.out of=$@ ibs=32 skip=1;rm -f a.out
	${LS} -l $@

sm_hp.o: hp.o
	rm -f sm_hp.c
	ln -s hp.c sm_hp.c
	${CC} -S ${SMOPTS} sm_hp.c
	${C2} -i sm_hp.s | as -o sm_hp.o
	rm -f sm_hp.s sm_hp.c

sm_up.o: up.o
	rm -f sm_up.c
	ln -s up.c sm_up.c
	${CC} -S ${SMOPTS} sm_up.c
	${C2} -i sm_up.s | as -o sm_up.o
	rm -f sm_up.s sm_up.c

sm_sys.o: ${STAND}/sys.c
	rm -f sm_sys.c
	ln -s ${STAND}/sys.c sm_sys.c
	${CC} -c -O ${SMOPTS} sm_sys.c
	rm -f sm_sys.c

sm_autoconf.o: autoconf.o
	rm -f sm_autoconf.c
	ln -s autoconf.c sm_autoconf.c
	${CC} -c -O ${SMOPTS} sm_autoconf.c
	rm -f sm_autoconf.c

confrk.o: confrk.c confxx.o
confhp.o: confhp.c confxx.o
confkra.o: confkra.c confxx.o
confup.o: confup.c confxx.o
confra.o: confra.c confxx.o
confrl.o: confrl.c confxx.o

confxx.o:
	touch confxx.o

confrk.c: confxx.c ${STAND}/dev.c
	sed -e 's/io->i_dev/0/g'		< ${STAND}/dev.c > confrk.c
	sed -e 's/XX/hk/' -e 's/xx/rk/g'	< confxx.c >> confrk.c
confhp.c: confxx.c ${STAND}/dev.c
	sed -e 's/io->i_dev/0/g'		< ${STAND}/dev.c > confhp.c
	sed -e 's/XX/hp/' -e 's/xx/hp/g'	< confxx.c >> confhp.c
confkra.c: confxx.c ${STAND}/dev.c
	sed -e 's/io->i_dev/0/g'		< ${STAND}/dev.c > confkra.c
	sed -e 's/XX/kra/' -e 's/xx/kra/g'	< confxx.c >> confkra.c
confup.c: confxx.c ${STAND}/dev.c
	sed -e 's/io->i_dev/0/g'		< ${STAND}/dev.c > confup.c
	sed -e 's/XX/up/' -e 's/xx/up/g'	< confxx.c >> confup.c
confra.c: confxx.c ${STAND}/dev.c
	sed -e 's/io->i_dev/0/g'		< ${STAND}/dev.c > confra.c
	sed -e 's/XX/ra/' -e 's/xx/ra/g'	< confxx.c >> confra.c
confrl.c: confxx.c ${STAND}/dev.c
	sed -e 's/io->i_dev/0/g'		< ${STAND}/dev.c > confrl.c
	sed -e 's/XX/rl/' -e 's/xx/rl/g'	< confxx.c >> confrl.c

clean:
	rm -f *.o *.exe *.i sm_*.c libsa.a
	rm -f boot[a-wyz][a-z].c conf[a-wyz][a-z].c confkra.c
	rm -f ${ALL} a.out core errs sboot bootconf.c

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${INCPATH} -DSTANDALONE ${SRCS} ${DUMMIES}

install: FRC
	install -c -s -o bin -g bin -m 644 \
	    format cat copy drtest ls ${DESTDIR}/stand
	[ -d ../vaxdist/tp ] || mkdir ../vaxdist/tp
	cp tpcopy ../vaxdist/tp/copy
	cp tpboot ../vaxdist/tp/boot
	cp tpformat ../vaxdist/tp/format
	cp boot a.out; strip a.out; \
		dd if=a.out of=../floppy/boot bs=32 skip=1; rm a.out
	cp 730boot a.out; strip a.out; \
		dd if=a.out of=../cassette/boot.730 bs=32 skip=1; rm a.out
	cp ../floppy/boot ../cassette/boot.750
	cp ../floppy/boot ../consolerl/boot
	cp copy a.out; strip a.out; \
		dd if=a.out of=../floppy/copy bs=32 skip=1; rm a.out
	cp 730copy a.out; strip a.out; \
		dd if=a.out of=../cassette/copy.730 bs=32 skip=1; rm a.out
	cp ../floppy/copy ../cassette/copy.750
	cp ../floppy/copy ../consolerl/copy
	cp format a.out; strip a.out; \
		dd if=a.out of=../floppy/format bs=32 skip=1; rm a.out
	cp 730format a.out; strip a.out; \
		dd if=a.out of=../cassette/format.730 bs=32 skip=1; rm a.out
	cp ../floppy/format ../cassette/format.750
	cp ../floppy/format ../consolerl/format
	cp drtest a.out; strip a.out; \
		dd if=a.out of=../floppy/drtest bs=32 skip=1; rm a.out
	cp 730drtest a.out; strip a.out; \
		dd if=a.out of=../cassette/drtest.730 bs=32 skip=1; rm a.out
	cp ../floppy/drtest ../cassette/drtest.750
	cp ../floppy/drtest ../consolerl/drtest
	install -c -o bin -g bin -m 444 ${MDEC} ${DESTDIR}/usr/mdec
	rm -f ${DESTDIR}/usr/mdec/bootrd
	ln ${DESTDIR}/usr/mdec/bootra ${DESTDIR}/usr/mdec/bootrd

lint: ${SRCS}
	lint ${COPTS} -hxbn boot.c ${SRCS} | \
	    sed -e '/possible pointer alignment/d' \
		-e '/struct\/union .* never defined/d'

tags: ${SRCS}
	ctags ${SRCS} ${DUMMIES}

FRC:
