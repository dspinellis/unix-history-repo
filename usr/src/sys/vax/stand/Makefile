#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	7.5 (Berkeley) %G%
#
DESTDIR=/
INCPATH=-I. -I../h
CFLAGS=	-O ${COPTS} 
COPTS=	${INCPATH} -DSTANDALONE -DCOMPAT_42 -DVAX780 -DVAX750 -DVAX730 -DVAX8600
730OPTS=-O ${INCPATH} -DSTANDALONE -DCOMPAT_42 -DVAX730
RELOC=	70000

SRCS=	boot.c bootxx.c format.c copy.c drtest.c \
	sys.c conf.c prf.c machdep.c \
	autoconf.c hp.c hpmaptype.c ht.c idc.c mba.c mt.c rk.c \
	rl.c tm.c ts.c up.c upmaptype.c uba.c uda.c ut.c \
	drtest.c format.c ../vax/dkbad.c
DUMMIES= confxx.c
DRIVERS=autoconf.o hp.o hpmaptype.o ht.o idc.o mba.o mt.o \
	rk.o rl.o tm.o ts.o \
	up.o upmaptype.o uba.o uda.o ut.o
# These drivers don't have ecc correction and bad sector forwarding;
# they are placed in the file system boot area for 750's.  If your
# root has bad sectors you can try and squeeze the newer drivers in...
ODRIVERS=sm_hp.o sm_up.o
LIBSA=	libsa.a

ALL=	boot tpboot copy tpcopy \
	format tpformat drtest boothp boothk bootup bootra bootrl \
	730boot 730copy 730format 730drtest

all: ${ALL}

${LIBSA}: sys.o conf.o ${DRIVERS} prf.o machdep.o dkbad.o
	ar crv ${LIBSA} $?
	ranlib ${LIBSA}

${DRIVERS}: savax.h
	cc -c -S ${COPTS} $*.c
	/lib/c2 -i $*.s | as -o $*.o
	rm -f $*.s

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

tpboot:	tpboot.o relsrt0.o ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o tpboot.o ${LIBSA} -lc
	strip a.out; dd if=a.out of=tpboot ibs=32 skip=1; rm -f a.out

tpboot.o: boot.c ../h/param.h ../h/inode.h ../h/fs.h
tpboot.o: saio.h ../h/reboot.h ../h/vm.h 
	cp boot.c tpboot.c; chmod +w tpboot.c
	cc -c ${CFLAGS} -DJUSTASK tpboot.c
	rm -f tpboot.c

tpcopy:	copy.o tpsrt0.o ${LIBSA}
	ld -N tpsrt0.o copy.o ${LIBSA} -lc
	strip a.out; dd if=a.out of=tpcopy ibs=32 skip=1; rm -f a.out

tpformat: format.o tpsrt0.o confhpup.o ${LIBSA}
	cp format.c tpformat.c; chmod +w tpformat.c
	cc -c ${CFLAGS} -DJUSTEXIT tpformat.c
	rm -f tpformat.c
	ld -N tpsrt0.o tpformat.o confhpup.o ${LIBSA} -lc
	strip a.out; dd if=a.out of=tpformat ibs=32 skip=1; rm -f a.out

# bootable from floppy or real disks

boot:	boot.o relsrt0.o bootconf.o ${LIBSA}
	ld -N -T ${RELOC} -o boot relsrt0.o boot.o bootconf.o ${LIBSA} -lc

bootconf.o: conf.o
	cp conf.c bootconf.c
	cc -c ${CFLAGS} -DBOOT bootconf.c
	rm -f bootconf.c

copy:	copy.o srt0.o conf.o ${LIBSA}
	ld -N -o copy srt0.o copy.o conf.o ${LIBSA} -lc

format:	format.o srt0.o confhpup.o ${LIBSA}
	ld -N -o format srt0.o format.o confhpup.o ${LIBSA} -lc

drtest:	drtest.o srt0.o confhpup.o ${LIBSA}
	ld -N -o drtest srt0.o drtest.o confhpup.o ${LIBSA} -lc

# for 730s minimize size to avoid microcode botch
# (won't load files larger than 12.5 Kbytes)

730boot.o: boot.o
	cp boot.c 730boot.c; chmod +w 730boot.c
	cc -c ${730OPTS} 730boot.c
	rm -f 730boot.c

730boot: 730boot.o relsrt0.o 730bootconf.o ${LIBSA}
	ld -N -T ${RELOC} -o 730boot relsrt0.o 730boot.o 730bootconf.o ${LIBSA} -lc

730bootconf.o: conf.o
	cp conf.c 730bootconf.c
	cc -c ${730OPTS} -DBOOT 730bootconf.c
	rm -f 730bootconf.c

730copy: copy.o srt0.o 730conf.o ${LIBSA}
	ld -N -o 730copy srt0.o copy.o 730conf.o ${LIBSA} -lc

730drtest: drtest.o srt0.o confup.o ${LIBSA}
	ld -N -o 730drtest srt0.o drtest.o confup.o ${LIBSA} -lc

730format: format.o srt0.o confup.o ${LIBSA}
	ld -N -o 730format srt0.o format.o confup.o ${LIBSA} -lc

730conf.o: conf.o
	cp conf.c 730conf.c
	cc -c ${730OPTS} 730conf.c
	rm -f 730conf.c

# bootstrap from ether

### not yet, rosin, not yet ###

# getting booted from disc

boothk: relsrt0.o bootxx.o confrk.o sm_sys.o ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o bootxx.o confrk.o sm_sys.o ${LIBSA} -lc
	size a.out
	strip a.out;dd if=a.out of=boothk ibs=32 skip=1;rm -f a.out
	ls -l boothk

boothp: relsrt0.o bootxx.o confhp.o sm_hp.o sm_sys.o ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o bootxx.o confhp.o sm_hp.o sm_sys.o ${LIBSA} -lc
	size a.out
	strip a.out;dd if=a.out of=boothp ibs=32 skip=1;rm -f a.out
	ls -l boothp

bootup: relsrt0.o bootxx.o confup.o sm_up.o sm_sys.o ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o bootxx.o confup.o sm_up.o sm_sys.o ${LIBSA} -lc
	size a.out
	strip a.out;dd if=a.out of=bootup ibs=32 skip=1;rm -f a.out
	ls -l bootup

bootra: relsrt0.o bootxx.o confra.o sm_sys.o ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o bootxx.o confra.o sm_sys.o ${LIBSA} -lc
	size a.out
	strip a.out;dd if=a.out of=bootra ibs=32 skip=1;rm -f a.out
	ls -l bootra

bootrl: relsrt0.o bootxx.o confrl.o sm_sys.o ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o bootxx.o confrl.o sm_sys.o ${LIBSA} -lc
	size a.out
	strip a.out;dd if=a.out of=bootrl ibs=32 skip=1;rm -f a.out
	ls -l bootrl

sm_hp.o: hp.o
	-rm -f sm_hp.c
	-ln -s hp.c sm_hp.c
	cc -S -DSMALL ${CFLAGS} sm_hp.c
	/lib/c2 -i sm_hp.s | as -o sm_hp.o
	rm -f sm_hp.s sm_hp.c

sm_up.o: up.o
	-rm -f sm_up.c
	-ln -s up.c sm_up.c
	cc -S -DSMALL ${CFLAGS} sm_up.c
	/lib/c2 -i sm_up.s | as -o sm_up.o
	rm -f sm_up.s sm_up.c

sm_sys.o: sys.o
	-rm -f sm_sys.c
	-ln -s sys.c sm_sys.c
	cc -c -DSMALL ${CFLAGS} sm_sys.c
	-rm -f sm_sys.c

confrk.o: confrk.c confxx.o
confhp.o: confhp.c confxx.o
confup.o: confup.c confxx.o
confra.o: confra.c confxx.o
confrl.o: confrl.c confxx.o

confxx.o:
	touch confxx.o

confrk.c: confxx.c
	-rm -f confrk.c
	sed -e 's/XX/hk/' -e 's/xx/rk/g' <confxx.c >confrk.c
confhp.c: confxx.c
	-rm -f confhp.c
	sed -e 's/XX/hp/' -e 's/xx/hp/g' <confxx.c >confhp.c
confup.c: confxx.c
	-rm -f confup.c
	sed -e 's/XX/up/' -e 's/xx/up/g' <confxx.c >confup.c
confra.c: confxx.c
	-rm -f confra.c
	sed -e 's/XX/ra/' -e 's/xx/ra/g' <confxx.c >confra.c
confrl.c: confxx.c
	-rm -f confrl.c
	sed -e 's/XX/rl/' -e 's/xx/rl/g' <confxx.c >confrl.c

# utilities

print:
	@pr makefile
	@ls -l | pr 
	@pr *.h *.c

clean:
	rm -f *.o *.exe *.i errs sm_*.c
	rm -f a.out boot cat tpboot tpcopy copy tpformat
	rm -f boot[a-z]? boot[a-wyz][a-z].c conf[a-wyz][a-z].c
	rm -f format drtest core sboot bootconf.c
	rm -f 730boot 730copy 730drtest 730format

lint:
	lint ${COPTS} -hxbn boot.c ${SRCS} | \
	    grep -v 'possible pointer alignment' | \
	    grep -v 'struct/union .* never defined'

install: ${ALL}
	cp boot ${DESTDIR}/boot
	cp format ${DESTDIR}/format
	cp copy ${DESTDIR}/copy
	cp drtest ${DESTDIR}/drtest
	strip ${DESTDIR}/format ${DESTDIR}/copy ${DESTDIR}/drtest
	cp pcs750.bin ${DESTDIR}/pcs750.bin
	cp tpcopy ../dist/tp/copy
	cp tpboot ../dist/tp/boot
	cp tpformat ../dist/tp/format
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
	cp bootup boothk boothp bootra bootrl ${DESTDIR}/usr/mdec

depend:
	for i in ${SRCS} ${DUMMIES}; do \
	    cc -M ${INCPATH} $$i | \
	    awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it


sys.o: sys.c ./../h/param.h /usr/include/machine/machparam.h ../h/signal.h
sys.o: /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h ./../h/dir.h
sys.o: ./saio.h

conf.o: conf.c ./../machine/pte.h ./../h/param.h
conf.o: /usr/include/machine/machparam.h ../h/signal.h /usr/include/sys/types.h
conf.o: ./../h/inode.h ./../h/fs.h ./../vaxmba/mbareg.h ./saio.h

prf.o: prf.c ./../h/param.h /usr/include/machine/machparam.h ../h/signal.h
prf.o: /usr/include/sys/types.h ./../vax/mtpr.h ./../vax/cons.h

machdep.o: machdep.c ./../h/param.h /usr/include/machine/machparam.h
machdep.o: ../h/signal.h /usr/include/sys/types.h ./../vax/mtpr.h

autoconf.o: autoconf.c ./../machine/pte.h ./../h/param.h
autoconf.o: /usr/include/machine/machparam.h ../h/signal.h
autoconf.o: /usr/include/sys/types.h ./../vax/cpu.h ./../vax/nexus.h
autoconf.o: ./../vaxuba/ubareg.h ./../vaxmba/mbareg.h ./../vax/mtpr.h ./savax.h

hp.o: hp.c ./../h/param.h /usr/include/machine/machparam.h ../h/signal.h
hp.o: /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h ./../h/dkbad.h
hp.o: ./../vax/pte.h ./../vaxmba/hpreg.h ./../vaxmba/mbareg.h ./saio.h
hp.o: ./savax.h

hpmaptype.o: hpmaptype.c ./../machine/pte.h ./../h/param.h
hpmaptype.o: /usr/include/machine/machparam.h ../h/signal.h
hpmaptype.o: /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h
hpmaptype.o: ./../vaxmba/hpreg.h ./../vaxmba/mbareg.h ./saio.h ./savax.h

ht.o: ht.c ./../machine/pte.h ./../h/param.h /usr/include/machine/machparam.h
ht.o: ../h/signal.h /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h
ht.o: ./../vaxmba/htreg.h ./../vaxmba/mbareg.h ./saio.h ./savax.h

idc.o: idc.c ./../machine/pte.h ./../h/param.h /usr/include/machine/machparam.h
idc.o: ../h/signal.h /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h
idc.o: ./../vaxuba/idcreg.h ./../vaxuba/ubareg.h ./saio.h ./savax.h

mba.o: mba.c ./../machine/pte.h ./../h/param.h /usr/include/machine/machparam.h
mba.o: ../h/signal.h /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h
mba.o: ./../h/vm.h /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
mba.o: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
mba.o: /usr/include/sys/vmsystm.h ./../vax/mtpr.h ./../vaxmba/mbareg.h
mba.o: ./../vaxmba/hpreg.h ./saio.h ./savax.h

mt.o: mt.c ./../machine/pte.h ./../h/param.h /usr/include/machine/machparam.h
mt.o: ../h/signal.h /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h
mt.o: ./../vaxmba/mtreg.h ./../vaxmba/mbareg.h ./saio.h ./savax.h

rk.o: rk.c ./../machine/pte.h ./../h/param.h /usr/include/machine/machparam.h
rk.o: ../h/signal.h /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h
rk.o: ./../vaxuba/ubareg.h ./../vaxuba/rkreg.h ./saio.h ./savax.h

rl.o: rl.c ./../machine/pte.h ./../h/param.h /usr/include/machine/machparam.h
rl.o: ../h/signal.h /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h
rl.o: ./../vaxuba/rlreg.h ./../vaxuba/ubareg.h ./saio.h ./savax.h

tm.o: tm.c ./../machine/pte.h ./../h/param.h /usr/include/machine/machparam.h
tm.o: ../h/signal.h /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h
tm.o: ./../vaxuba/ubareg.h ./../vaxuba/tmreg.h ./saio.h ./savax.h

ts.o: ts.c ./../machine/pte.h ./../h/param.h /usr/include/machine/machparam.h
ts.o: ../h/signal.h /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h
ts.o: ./../vaxuba/tsreg.h ./../vaxuba/ubareg.h ./saio.h ./savax.h

up.o: up.c ./../h/param.h /usr/include/machine/machparam.h ../h/signal.h
up.o: /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h ./../h/dkbad.h
up.o: ./../h/vmmac.h ./../vax/pte.h ./../vaxuba/upreg.h ./../vaxuba/ubareg.h
up.o: ./saio.h ./savax.h

upmaptype.o: upmaptype.c ./../h/param.h /usr/include/machine/machparam.h
upmaptype.o: ../h/signal.h /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h
upmaptype.o: ./../h/dkbad.h ./../h/vmmac.h ./../vax/pte.h ./../vaxuba/upreg.h
upmaptype.o: ./../vaxuba/ubareg.h ./saio.h ./savax.h

uba.o: uba.c ./../machine/pte.h ./../h/param.h /usr/include/machine/machparam.h
uba.o: ../h/signal.h /usr/include/sys/types.h ./../h/inode.h ./../h/vm.h
uba.o: /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
uba.o: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
uba.o: /usr/include/sys/vmsystm.h ./../h/fs.h ./../vax/cpu.h
uba.o: ./../vaxuba/ubareg.h ./saio.h ./savax.h

uda.o: uda.c ./../machine/pte.h ./../h/param.h /usr/include/machine/machparam.h
uda.o: ../h/signal.h /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h
uda.o: ./saio.h ./savax.h ./../vaxuba/udareg.h ./../vaxuba/ubareg.h
uda.o: ./../vax/mscp.h

ut.o: ut.c ./../machine/pte.h ./../h/param.h /usr/include/machine/machparam.h
ut.o: ../h/signal.h /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h
ut.o: ./../vaxuba/ubareg.h ./../vaxuba/utreg.h ./saio.h ./savax.h

drtest.o: drtest.c ./../h/param.h /usr/include/machine/machparam.h
drtest.o: ../h/signal.h /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h
drtest.o: ./saio.h

format.o: format.c ./../h/param.h /usr/include/machine/machparam.h
format.o: ../h/signal.h /usr/include/sys/types.h ./../h/fs.h ./../h/inode.h
format.o: ./../h/dkbad.h ./../h/vmmac.h ./saio.h ./savax.h

dkbad.o: ../vax/dkbad.c ../h/param.h /usr/include/machine/machparam.h
dkbad.o: ../h/signal.h /usr/include/sys/types.h ../h/buf.h ../h/dkbad.h

bootxx.o: bootxx.c ./../h/param.h /usr/include/machine/machparam.h
bootxx.o: ../h/signal.h /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h
bootxx.o: ./../h/vm.h /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
bootxx.o: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
bootxx.o: /usr/include/sys/vmsystm.h /usr/include/a.out.h
bootxx.o: /usr/include/sys/exec.h ./saio.h ./../h/reboot.h

confxx.o: confxx.c ./../machine/pte.h ./../h/param.h
confxx.o: /usr/include/machine/machparam.h ../h/signal.h
confxx.o: /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h ./saio.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
