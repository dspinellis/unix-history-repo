#	Makefile	4.27	83/02/18

DESTDIR=/
CFLAGS=	-O -DSTANDALONE ${COPTS} 
COPTS=	-DVAX780 -DVAX750 -DVAX730
730OPTS=-O -DVAX730
RELOC=	70000
SRCS=	sys.c conf.c prf.c machdep.c \
	autoconf.c hp.c hpmaptype.c ht.c idc.c mba.c mt.c rk.c \
	rl.c tm.c ts.c \
	up.c upmaptype.c uba.c uda.c ut.c \
	drtest.c format.c up.new.c
DRIVERS=autoconf.o hp.o hpmaptype.o ht.o idc.o mba.o mt.o \
	rk.o rl.o tm.o ts.o \
	up.o upmaptype.o uba.o uda.o ut.o
NEWDRIVERS=hp.new.o up.new.o

ALL=	/usr/lib/libsa.a srt0.o boot tpboot copy tpcopy \
	format tpformat drtest boothp boothk bootup bootra bootrl \
	730boot 730copy

all: ${ALL}

/usr/lib/libsa.a: sys.o conf.o ${DRIVERS} prf.o machdep.o
	ar crv /usr/lib/libsa.a $?
	ranlib /usr/lib/libsa.a

${NEWDRIVERS} ${DRIVERS}: savax.h
	cc -c -S ${COPTS} $*.c
	/lib/c2 -i $*.s | as -o $*.o
	rm $*.s

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

tpcopy:	copy.c tpsrt0.o /usr/lib/libsa.a
	cp copy.c tpcopy.c; chmod +w tpcopy.c
	cc -c -O tpcopy.c
	rm tpcopy.c
	ld -N tpsrt0.o tpcopy.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=tpcopy ibs=32 skip=1; rm b.out

tpformat: format.c tpsrt0.o /usr/lib/libsa.a
	cp format.c tpformat.c; chmod +w tpformat.c
	cc -c -O -DJUSTEXIT tpformat.c
	rm tpformat.c
	ld -N tpsrt0.o tpformat.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=tpformat ibs=32 skip=1; rm b.out

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

format:	format.o srt0.o dkbad.o
format:	confhpup.o hp.new.o up.new.o /usr/lib/libsa.a
	ld -N -o format srt0.o format.o dkbad.o confhpup.o \
		hp.new.o up.new.o -lsa -lc

drtest:	drtest.o srt0.o dkbad.o
drtest:	confhpup.o hp.new.o up.new.o /usr/lib/libsa.a
	ld -N -o drtest srt0.o drtest.o dkbad.o confhpup.o \
		hp.new.o up.new.o -lsa -lc

dkbad.o: ../vax/dkbad.c
	${CC} -c ${CFLAGS} ../vax/dkbad.c

# for 730s minimize size so it fits on the cassette

730boot: boot.o relsrt0.o 730bootconf.o /usr/lib/libsa.a
	ld -N -T ${RELOC} -o 730boot relsrt0.o boot.o 730bootconf.o -lsa -lc

730bootconf.o: conf.c ../h/param.h ../h/inode.h ../machine/pte.h
730bootconf.o: ../h/fs.h saio.h
	cp conf.c 730bootconf.c
	cc -c ${730OPTS} -DBOOT 730bootconf.c
	rm 730bootconf.c

730copy: copy.o srt0.o 730conf.o /usr/lib/libsa.a
	ld -N -o 730copy srt0.o copy.o 730conf.o -lsa -lc

730conf.o: conf.c ../h/param.h ../h/inode.h ../machine/pte.h
730conf.o: ../h/fs.h saio.h
	cp conf.c 730conf.c
	cc -c ${730OPTS} -DBOOT 730conf.c
	rm 730conf.c

# bootstrap from ether

### not yet, rosin, not yet ###

# getting booted from disc

boothk: relsrt0.o boothk.o confrk.o /usr/lib/libsa.a
	ld -N -T ${RELOC} relsrt0.o boothk.o confrk.o -lsa -lc
	cp a.out b.out;strip b.out;dd if=b.out of=boothk ibs=32 skip=1;rm b.out

boothk.o: bootxx.c
	cc -c -S ${COPTS} -DBOOTRK bootxx.c
	/lib/c2 -i bootxx.s | as -o boothk.o
	rm -f bootxx.s

boothp: relsrt0.o boothp.o confhp.o /usr/lib/libsa.a
	ld -N -T ${RELOC} relsrt0.o boothp.o confhp.o -lsa -lc
	cp a.out b.out;strip b.out;dd if=b.out of=boothp ibs=32 skip=1;rm b.out

boothp.o: bootxx.c
	cc -c -S ${COPTS} -DBOOTHP bootxx.c
	/lib/c2 -i bootxx.s | as -o boothp.o
	rm -f bootxx.s

bootup: relsrt0.o bootup.o confup.o /usr/lib/libsa.a
	ld -N -T ${RELOC} relsrt0.o bootup.o confup.o -lsa -lc
	cp a.out b.out;strip b.out;dd if=b.out of=bootup ibs=32 skip=1;rm b.out

bootup.o: bootxx.c
	cc -c -S ${COPTS} -DBOOTUP bootxx.c
	/lib/c2 -i bootxx.s | as -o bootup.o
	rm -f bootxx.s

bootra: relsrt0.o bootra.o confra.o /usr/lib/libsa.a
	ld -N -T ${RELOC} relsrt0.o bootra.o confra.o -lsa -lc
	cp a.out b.out;strip b.out;dd if=b.out of=bootra ibs=32 skip=1;rm b.out

bootra.o: bootxx.c
	cc -c -S ${COPTS} -DBOOTRA bootxx.c
	/lib/c2 -i bootxx.s | as -o bootra.o
	rm -f bootxx.s

bootrl: relsrt0.o bootrl.o confrl.o /usr/lib/libsa.a
	ld -N -T ${RELOC} relsrt0.o bootrl.o confra.o -lsa -lc
	cp a.out b.out;strip b.out;dd if=b.out of=bootrl ibs=32 skip=1;rm b.out

bootrl.o: bootxx.c
	cc -c -S ${COPTS} -DBOOTRL bootxx.c
	/lib/c2 -i bootxx.s | as -o bootrl.o
	rm -f bootxx.s

boothp.o: bootxx.c ../h/param.h ../h/inode.h ../machine/pte.h ../h/reboot.h
boothp.o: ../h/fs.h saio.h
boothk.o: bootxx.c ../h/param.h ../h/inode.h ../machine/pte.h ../h/reboot.h
boothk.o: ../h/fs.h saio.h
bootup.o: bootxx.c ../h/param.h ../h/inode.h ../machine/pte.h ../h/reboot.h
bootup.o: ../h/fs.h saio.h
bootra.o: bootxx.c ../h/param.h ../h/inode.h ../machine/pte.h ../h/reboot.h
bootra.o: ../h/fs.h saio.h
bootrl.o: bootxx.c ../h/param.h ../h/inode.h ../machine/pte.h ../h/reboot.h
bootrl.o: ../h/fs.h saio.h

confrk.o: confrk.c ../h/param.h ../h/fs.h ../machine/pte.h ../h/inode.h saio.h
	${CC} -c ${CFLAGS} confrk.c
confhp.o: confhp.c ../h/param.h ../h/fs.h ../machine/pte.h ../h/inode.h saio.h
	${CC} -c ${CFLAGS} confhp.c
confup.o: confup.c ../h/param.h ../h/fs.h ../machine/pte.h ../h/inode.h saio.h
	${CC} -c ${CFLAGS} confup.c
confra.o: confra.c ../h/param.h ../h/fs.h ../machine/pte.h ../h/inode.h saio.h
	${CC} -c ${CFLAGS} confra.c
confrl.o: confrl.c ../h/param.h ../h/fs.h ../machine/pte.h ../h/inode.h saio.h
	${CC} -c ${CFLAGS} confrl.c

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
	rm -f boot[a-z]? conf[a-wyz][a-z].c
	rm -f format drtest core sboot bootconf.c
	rm -f 730boot 730copy

lint:
	lint ${COPTS} -hxbn boot.c ${SRCS} | \
	    grep -v 'possible pointer alignment' | \
	    grep -v 'struct/union .* never defined'

install: ${ALL}
	cp tpcopy ${DESTDIR}/tp/copy
	cp tpboot ${DESTDIR}/tp/boot
	cp tpformat ${DESTDIR}/tp/format
	cp copy a.out; strip a.out; dd if=a.out of=../floppy/copy bs=32 skip=1
	cp boot a.out; strip a.out; dd if=a.out of=../floppy/boot bs=32 skip=1
	cp 730boot a.out; strip a.out; \
		dd if=a.out of=../cassette/boot bs=32 skip=1
	cp 730copy a.out; strip a.out; \
		dd if=a.out of=../cassette/copy bs=32 skip=1
	cp format a.out; strip a.out; \
		dd if=a.out of=../floppy/format bs=32 skip=1
	cp ../floppy/format ../cassette/format
	cp drtest a.out; strip a.out; \
		dd if=a.out of=../floppy/drtest bs=32 skip=1
	cp ../floppy/drtest ../cassette/drtest
	cp bootup boothk boothp bootra bootrl ${DESTDIR}/usr/mdec

# beware...
