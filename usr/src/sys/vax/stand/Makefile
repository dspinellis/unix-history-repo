#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.4 (Berkeley) %G%
#
DESTDIR=/
CFLAGS=	-O -I../h -DSTANDALONE ${COPTS} 
COPTS=	-DVAX780 -DVAX750 -DVAX730
730OPTS=-O -DSTANDALONE -DVAX730
RELOC=	70000
SRCS=	sys.c conf.c prf.c machdep.c \
	autoconf.c hp.c hpmaptype.c ht.c idc.c mba.c mt.c rk.c \
	rl.c tm.c ts.c \
	up.c upmaptype.c uba.c uda.c ut.c \
	drtest.c format.c up.old.c hp.old.c
DRIVERS=autoconf.o hp.o hpmaptype.o ht.o idc.o mba.o mt.o \
	rk.o rl.o tm.o ts.o \
	up.o upmaptype.o uba.o uda.o ut.o
# These drivers don't have ecc correction and bad sector forwarding;
# they are placed in the file system boot area for 750's.  If your
# root has bad sectors you can try and squeeze the newer drivers in...
ODRIVERS=hp.old.o up.old.o

ALL=	/usr/lib/libsa.a srt0.o boot tpboot copy tpcopy \
	format tpformat drtest boothp boothk bootup bootra bootrl \
	730boot 730copy 730format 730drtest

all: ${ALL}

/usr/lib/libsa.a: sys.o conf.o ${DRIVERS} prf.o machdep.o dkbad.o
	ar crv /usr/lib/libsa.a $?
	ranlib /usr/lib/libsa.a

${ODRIVERS} ${DRIVERS}: savax.h
	cc -c -S ${COPTS} $*.c
	/lib/c2 -i $*.s | as -o $*.o
	rm $*.s

dkbad.o: ../vax/dkbad.c
	${CC} -c ${CFLAGS} ../vax/dkbad.c

# startups

srt0.o: srt0.c ../vax/mtpr.h ../vax/cpu.h
	cc -E -DRELOC=0x${RELOC} ${COPTS} srt0.c | as -o srt0.o

tpsrt0.o: srt0.c ../vax/mtpr.h ../vax/cpu.h
	cc -E -DRELOC=0x${RELOC} -DTP ${COPTS} srt0.c | as -o tpsrt0.o 

relsrt0.o: srt0.c ../vax/mtpr.h ../vax/cpu.h
	cc -E -DRELOC=0x${RELOC} -DREL ${COPTS} srt0.c | as -o relsrt0.o

# bootable from tape

tpboot:	tpboot.o relsrt0.o /usr/lib/libsa.a
	ld -N -T ${RELOC} relsrt0.o tpboot.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=tpboot ibs=32 skip=1; rm b.out

tpboot.o: boot.c ../h/param.h ../h/inode.h ../h/fs.h
tpboot.o: saio.h ../h/reboot.h ../h/vm.h 
	cp boot.c tpboot.c; chmod +w tpboot.c
	cc -c -O -DJUSTASK tpboot.c
	rm tpboot.c

tpcopy:	copy.o tpsrt0.o /usr/lib/libsa.a
	ld -N tpsrt0.o copy.o -lsa -lc
	cp a.out b.out; strip b.out; \
		dd if=b.out of=tpcopy ibs=32 skip=1; rm b.out

tpformat: format.o tpsrt0.o confhpup.o /usr/lib/libsa.a
	cp format.c tpformat.c; chmod +w tpformat.c
	cc -c -O -DJUSTEXIT tpformat.c
	rm tpformat.c
	ld -N tpsrt0.o tpformat.o confhpup.o -lsa -lc
	cp a.out b.out; strip b.out; \
		dd if=b.out of=tpformat ibs=32 skip=1; rm b.out

# bootable from floppy or real disks

boot:	boot.o relsrt0.o bootconf.o /usr/lib/libsa.a
	ld -N -T ${RELOC} -o boot relsrt0.o boot.o bootconf.o -lsa -lc

bootconf.o: conf.c ../h/param.h ../h/inode.h ../machine/pte.h
bootconf.o: ../h/fs.h saio.h ../vaxmba/mbareg.h
	cp conf.c bootconf.c
	cc -c ${COPTS} -DBOOT bootconf.c
	rm bootconf.c

copy:	copy.o srt0.o conf.o /usr/lib/libsa.a
	ld -N -o copy srt0.o copy.o conf.o -lsa -lc

format:	format.o srt0.o confhpup.o /usr/lib/libsa.a
	ld -N -o format srt0.o format.o confhpup.o -lsa -lc

drtest:	drtest.o srt0.o confhpup.o /usr/lib/libsa.a
	ld -N -o drtest srt0.o drtest.o confhpup.o -lsa -lc

# for 730s minimize size to avoid microcode botch
# (won't load files larger than 12.5 Kbytes)

730boot.o: boot.c ../h/param.h ../h/inode.h ../h/fs.h
730boot.o: saio.h ../h/reboot.h ../h/vm.h 
	cp boot.c 730boot.c; chmod +w 730boot.c
	cc -c ${730OPTS} 730boot.c
	rm 730boot.c

730boot: 730boot.o relsrt0.o 730bootconf.o /usr/lib/libsa.a
	ld -N -T ${RELOC} -o 730boot relsrt0.o 730boot.o 730bootconf.o -lsa -lc

730bootconf.o: conf.c ../h/param.h ../h/inode.h ../machine/pte.h
730bootconf.o: ../h/fs.h saio.h
	cp conf.c 730bootconf.c
	cc -c ${730OPTS} -DBOOT 730bootconf.c
	rm -f 730bootconf.c

730copy: copy.o srt0.o 730conf.o /usr/lib/libsa.a
	ld -N -o 730copy srt0.o copy.o 730conf.o -lsa -lc

730drtest: drtest.o srt0.o confup.o /usr/lib/libsa.a
	ld -N -o 730drtest srt0.o drtest.o confup.o -lsa -lc

730format: format.o srt0.o confup.o /usr/lib/libsa.a
	ld -N -o 730format srt0.o format.o confup.o -lsa -lc

730conf.o: conf.c ../h/param.h ../h/inode.h ../machine/pte.h
730conf.o: ../h/fs.h saio.h
	cp conf.c 730conf.c
	cc -c ${730OPTS} 730conf.c
	rm -f 730conf.c

# bootstrap from ether

### not yet, rosin, not yet ###

# getting booted from disc

boothk: relsrt0.o boothk.o confrk.o /usr/lib/libsa.a
	ld -N -T ${RELOC} relsrt0.o boothk.o confrk.o -lsa -lc
	cp a.out b.out;strip b.out;dd if=b.out of=boothk ibs=32 skip=1;rm b.out

boothp: relsrt0.o boothp.o confhp.o hp.old.o /usr/lib/libsa.a
	ld -N -T ${RELOC} relsrt0.o boothp.o confhp.o hp.old.o -lsa -lc
	cp a.out b.out;strip b.out;dd if=b.out of=boothp ibs=32 skip=1;rm b.out

bootup: relsrt0.o bootup.o confup.o up.old.o /usr/lib/libsa.a
	ld -N -T ${RELOC} relsrt0.o bootup.o confup.o up.old.o -lsa -lc
	cp a.out b.out;strip b.out;dd if=b.out of=bootup ibs=32 skip=1;rm b.out

bootra: relsrt0.o bootra.o confra.o /usr/lib/libsa.a
	ld -N -T ${RELOC} relsrt0.o bootra.o confra.o -lsa -lc
	cp a.out b.out;strip b.out;dd if=b.out of=bootra ibs=32 skip=1;rm b.out

bootrl: relsrt0.o bootrl.o confrl.o /usr/lib/libsa.a
	ld -N -T ${RELOC} relsrt0.o bootrl.o confrl.o -lsa -lc
	cp a.out b.out;strip b.out;dd if=b.out of=bootrl ibs=32 skip=1;rm b.out

boothp.o: boothp.c ../h/param.h ../h/inode.h ../machine/pte.h ../h/reboot.h
boothp.o: ../h/fs.h saio.h
boothk.o: boothk.c ../h/param.h ../h/inode.h ../machine/pte.h ../h/reboot.h
boothk.o: ../h/fs.h saio.h
bootup.o: bootup.c ../h/param.h ../h/inode.h ../machine/pte.h ../h/reboot.h
bootup.o: ../h/fs.h saio.h
bootra.o: bootra.c ../h/param.h ../h/inode.h ../machine/pte.h ../h/reboot.h
bootra.o: ../h/fs.h saio.h
bootrl.o: bootrl.c ../h/param.h ../h/inode.h ../machine/pte.h ../h/reboot.h
bootrl.o: ../h/fs.h saio.h

boothk.c: bootxx.c
	sed -e 's/xx/hk/g' <bootxx.c >boothk.c
boothp.c: bootxx.c
	sed -e 's/xx/hp/g' <bootxx.c >boothp.c
bootup.c: bootxx.c
	sed -e 's/xx/up/g' <bootxx.c >bootup.c
bootra.c: bootxx.c
	sed -e 's/xx/ra/g' <bootxx.c >bootra.c
bootrl.c: bootxx.c
	sed -e 's/xx/rl/g' <bootxx.c >bootrl.c

confrk.o: confrk.c ../h/param.h ../h/fs.h ../machine/pte.h ../h/inode.h saio.h
confhp.o: confhp.c ../h/param.h ../h/fs.h ../machine/pte.h ../h/inode.h saio.h
confup.o: confup.c ../h/param.h ../h/fs.h ../machine/pte.h ../h/inode.h saio.h
confra.o: confra.c ../h/param.h ../h/fs.h ../machine/pte.h ../h/inode.h saio.h
confrl.o: confrl.c ../h/param.h ../h/fs.h ../machine/pte.h ../h/inode.h saio.h

confrk.c: confxx.c
	sed -e 's/XX/hk/' -e 's/xx/rk/g' <confxx.c >confrk.c
confhp.c: confxx.c
	sed -e 's/XX/hp/' -e 's/xx/hp/g' <confxx.c >confhp.c
confup.c: confxx.c
	sed -e 's/XX/up/' -e 's/xx/up/g' <confxx.c >confup.c
confra.c: confxx.c
	sed -e 's/XX/ra/' -e 's/xx/ra/g' <confxx.c >confra.c
confrl.c: confxx.c
	sed -e 's/XX/rl/' -e 's/xx/rl/g' <confxx.c >confrl.c

# utilities

print:
	@pr makefile
	@ls -l | pr 
	@pr *.h *.c

clean:
	rm -f *.o *.exe *.i errs
	rm -f a.out b.out boot cat tpboot tpcopy copy tpformat
	rm -f boot[a-z]? boot[a-wyz][a-z].c conf[a-wyz][a-z].c
	rm -f format drtest core sboot bootconf.c
	rm -f 730boot 730copy 730drtest 730format

lint:
	lint ${COPTS} -hxbn boot.c ${SRCS} | \
	    grep -v 'possible pointer alignment' | \
	    grep -v 'struct/union .* never defined'

install: ${ALL}
	cp tpcopy ${DESTDIR}/tp/copy
	cp tpboot ${DESTDIR}/tp/boot
	cp tpformat ${DESTDIR}/tp/format
	cp boot a.out; strip a.out; \
		dd if=a.out of=../floppy/boot bs=32 skip=1
	cp 730boot a.out; strip a.out; \
		dd if=a.out of=../cassette/boot.730 bs=32 skip=1
	cp ../floppy/boot ../cassette/boot.750
	cp copy a.out; strip a.out; \
		dd if=a.out of=../floppy/copy bs=32 skip=1
	cp 730copy a.out; strip a.out; \
		dd if=a.out of=../cassette/copy.730 bs=32 skip=1
	cp ../floppy/copy ../cassette/copy.750
	cp format a.out; strip a.out; \
		dd if=a.out of=../floppy/format bs=32 skip=1
	cp 730format a.out; strip a.out; \
		dd if=a.out of=../cassette/format.730 bs=32 skip=1
	cp ../floppy/format ../cassette/format.750
	cp drtest a.out; strip a.out; \
		dd if=a.out of=../floppy/drtest bs=32 skip=1
	cp 730drtest a.out; strip a.out; \
		dd if=a.out of=../cassette/drtest.730 bs=32 skip=1
	cp ../floppy/drtest ../cassette/drtest.750
	cp bootup boothk boothp bootra bootrl ${DESTDIR}/usr/mdec

# beware...
