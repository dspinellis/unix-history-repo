#
# Copyright (c) 1980, 1988 Regents of the University of California.
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
#	@(#)Makefile	7.15 (Berkeley) 7/9/88
#

DESTDIR=
STAND=	/stand
INCPATH=-I. -I../stand -I../h
VPATH=	../stand:../vax
DEFS=	${INCPATH} -DSTANDALONE -DCOMPAT_42
MACH=	-DVAX8600 -DVAX8200 -DVAX780 -DVAX750 -DVAX730 -DVAX630

# Collections of C options:
#	COPTS	device drivers/assembler, not optimized
#	SMOPTS	"small" versions for 7.5K bootxx programs
#	730OPTS	"small" versions for 11/730-only versions
#	CFLAGS	everything else

COPTS=	${DEFS} ${MACH}
CFLAGS=	-O ${DEFS} ${MACH}
730OPTS=-O ${DEFS} -DVAX730
SMOPTS=	${DEFS} -DSMALL -DVAX750 -DVAX630

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
	/lib/c2 -i $*.s | as -o $*.o
	rm -f $*.s

# startups

srt0.o: srt0.c ../vax/mtpr.h ../vax/cpu.h
	${CC} -E -DRELOC=0x${RELOC} ${COPTS} srt0.c | as -o srt0.o

tpsrt0.o: srt0.c ../vax/mtpr.h ../vax/cpu.h
	${CC} -E -DRELOC=0x${RELOC} -DTP ${COPTS} srt0.c | as -o tpsrt0.o

relsrt0.o: srt0.c ../vax/mtpr.h ../vax/cpu.h
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

ls: ls.o srt0.o getfile.o ${LIBSA}
	ld -N -o $@ srt0.o ls.o getfile.o ${LIBSA} -lc

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
	ls -l $@

boothp: relsrt0.o confhp.o sm_hp.o ${SMOBJ} ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o confhp.o sm_hp.o ${SMOBJ} ${LIBSA} -lc
	size a.out
	rm -f $@
	strip a.out;dd if=a.out of=$@ ibs=32 skip=1;rm -f a.out
	ls -l $@

bootkra: relsrt0.o confkra.o ${SMOBJ} ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o confkra.o ${SMOBJ} ${LIBSA} -lc
	size a.out
	rm -f $@
	strip a.out;dd if=a.out of=$@ ibs=32 skip=1;rm -f a.out
	ls -l $@

bootup: relsrt0.o confup.o sm_up.o ${SMOBJ} ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o confup.o sm_up.o ${SMOBJ} ${LIBSA} -lc
	size a.out
	rm -f $@
	strip a.out;dd if=a.out of=$@ ibs=32 skip=1;rm -f a.out
	ls -l $@

bootra: relsrt0.o confra.o ${SMOBJ} ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o confra.o ${SMOBJ} ${LIBSA} -lc
	size a.out
	rm -f $@
	strip a.out;dd if=a.out of=$@ ibs=32 skip=1;rm -f a.out
	ls -l $@

bootrl: relsrt0.o confrl.o ${SMOBJ} ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o confrl.o ${SMOBJ} ${LIBSA} -lc
	size a.out
	rm -f $@
	strip a.out;dd if=a.out of=$@ ibs=32 skip=1;rm -f a.out
	ls -l $@

sm_hp.o: hp.o
	rm -f sm_hp.c
	ln -s hp.c sm_hp.c
	${CC} -S ${SMOPTS} sm_hp.c
	/lib/c2 -i sm_hp.s | as -o sm_hp.o
	rm -f sm_hp.s sm_hp.c

sm_up.o: up.o
	rm -f sm_up.c
	ln -s up.c sm_up.c
	${CC} -S ${SMOPTS} sm_up.c
	/lib/c2 -i sm_up.s | as -o sm_up.o
	rm -f sm_up.s sm_up.c

sm_sys.o: sys.o
	rm -f sm_sys.c
	ln -s sys.c sm_sys.c
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

confrk.c: confxx.c dev.c
	sed -e 's/io->i_ino.i_dev/0/g'		< dev.c > confrk.c
	sed -e 's/XX/hk/' -e 's/xx/rk/g'	< confxx.c >> confrk.c
confhp.c: confxx.c dev.c
	sed -e 's/io->i_ino.i_dev/0/g'		< dev.c > confhp.c
	sed -e 's/XX/hp/' -e 's/xx/hp/g'	< confxx.c >> confhp.c
confkra.c: confxx.c dev.c
	sed -e 's/io->i_ino.i_dev/0/g'		< dev.c > confkra.c
	sed -e 's/XX/kra/' -e 's/xx/kra/g'	< confxx.c >> confkra.c
confup.c: confxx.c dev.c
	sed -e 's/io->i_ino.i_dev/0/g'		< dev.c > confup.c
	sed -e 's/XX/up/' -e 's/xx/up/g'	< confxx.c >> confup.c
confra.c: confxx.c dev.c
	sed -e 's/io->i_ino.i_dev/0/g'		< dev.c > confra.c
	sed -e 's/XX/ra/' -e 's/xx/ra/g'	< confxx.c >> confra.c
confrl.c: confxx.c dev.c
	sed -e 's/io->i_ino.i_dev/0/g'		< dev.c > confrl.c
	sed -e 's/XX/rl/' -e 's/xx/rl/g'	< confxx.c >> confrl.c

clean: FRC
	rm -f *.o *.exe *.i sm_*.c libsa.a
	rm -f boot[a-wyz][a-z].c conf[a-wyz][a-z].c
	rm -f ${ALL} a.out core errs sboot bootconf.c

depend: ${SRCS} FRC
	mkdep ${INCPATH} -DSTANDALONE ${SRCS} ${DUMMIES}

install: FRC
	install -c -s -o bin -g bin -m 644 format cat copy drtest ls ${DESTDIR}${STAND}
	install -c -o bin -g bin -m 644 boot pcs750.bin ${DESTDIR}/
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

lint:
	lint ${COPTS} -hxbn boot.c ${SRCS} | \
	    sed -e '/possible pointer alignment/d' \
		-e '/struct\/union .* never defined/d'

tags: ${SRCS} FRC
	ctags ${SRCS} ${DUMMIES}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

autoconf.o: autoconf.c ../machine/pte.h ../h/param.h /usr/include/sys/types.h
autoconf.o: ../h/signal.h /usr/include/machine/trap.h
autoconf.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
autoconf.o: ../vax/cpu.h ../vax/nexus.h ../vaxuba/ubareg.h ../vaxmba/mbareg.h
autoconf.o: ../vax/mtpr.h savax.h
boot.o: boot.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
boot.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
boot.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../h/vm.h
boot.o: ../h/vmparam.h ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
boot.o: ../h/vmsystm.h ../h/reboot.h /usr/include/a.out.h
boot.o: /usr/include/sys/exec.h ../stand/saio.h ../stand/saioctl.h
boot.o: ../stand/saerrno.h
bootxx.o: bootxx.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
bootxx.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
bootxx.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../h/vm.h
bootxx.o: ../h/vmparam.h ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
bootxx.o: ../h/vmsystm.h ../h/reboot.h /usr/include/a.out.h
bootxx.o: /usr/include/sys/exec.h ../stand/saio.h ../stand/saioctl.h
bootxx.o: ../stand/saerrno.h
cat.o: ../stand/cat.c
copy.o: ../stand/copy.c
dev.o: ../stand/dev.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
dev.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
dev.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../stand/saio.h
dev.o: ../stand/saioctl.h ../stand/saerrno.h
drtest.o: drtest.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
drtest.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
drtest.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../h/disklabel.h
drtest.o: ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h drtest.c
drtest.o: ../h/param.h /usr/include/sys/types.h ../h/signal.h
drtest.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
drtest.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../h/disklabel.h
drtest.o: ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h
format.o: format.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
format.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
format.o: /usr/include/machine/endian.h ../h/fs.h ../h/inode.h ../h/dkbad.h
format.o: ../h/vmmac.h ../h/disklabel.h ../vax/cpu.h ../vax/mtpr.h
format.o: ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h savax.h
format.o: format.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
format.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
format.o: /usr/include/machine/endian.h ../h/fs.h ../h/inode.h ../h/dkbad.h
format.o: ../h/vmmac.h ../h/disklabel.h ../vax/cpu.h ../vax/mtpr.h
format.o: ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h savax.h
getfile.o: ../stand/getfile.c ../h/param.h /usr/include/sys/types.h
getfile.o: ../h/signal.h /usr/include/machine/trap.h
getfile.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
getfile.o: ../h/inode.h ../h/fs.h ../stand/saio.h ../stand/saioctl.h
getfile.o: ../stand/saerrno.h
gets.o: ../stand/gets.c
hp.o: hp.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
hp.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
hp.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../h/dkbad.h
hp.o: ../h/disklabel.h ../vax/pte.h ../vaxmba/hpreg.h ../vaxmba/mbareg.h
hp.o: ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h savax.h
hpmaptype.o: hpmaptype.c
ht.o: ht.c ../machine/pte.h ../h/param.h /usr/include/sys/types.h ../h/signal.h
ht.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
ht.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../vaxmba/htreg.h
ht.o: ../vaxmba/mbareg.h ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h
ht.o: savax.h
idc.o: idc.c ../machine/pte.h ../h/param.h /usr/include/sys/types.h
idc.o: ../h/signal.h /usr/include/machine/trap.h
idc.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
idc.o: ../h/inode.h ../h/fs.h ../vaxuba/idcreg.h ../vaxuba/ubareg.h
idc.o: ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h savax.h
machdep.o: machdep.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
machdep.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
machdep.o: /usr/include/machine/endian.h ../vax/mtpr.h
mba.o: mba.c ../machine/pte.h ../h/param.h /usr/include/sys/types.h
mba.o: ../h/signal.h /usr/include/machine/trap.h
mba.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
mba.o: ../h/inode.h ../h/fs.h ../h/vm.h ../h/vmparam.h
mba.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
mba.o: ../vax/mtpr.h ../vaxmba/mbareg.h ../vaxmba/hpreg.h ../stand/saio.h
mba.o: ../stand/saioctl.h ../stand/saerrno.h savax.h
mt.o: mt.c ../machine/pte.h ../h/param.h /usr/include/sys/types.h ../h/signal.h
mt.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
mt.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../vaxmba/mtreg.h
mt.o: ../vaxmba/mbareg.h ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h
mt.o: savax.h
prf.o: prf.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
prf.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
prf.o: /usr/include/machine/endian.h ../vax/mtpr.h ../vax/cons.h
printn.o: ../stand/printn.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
printn.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
printn.o: /usr/include/machine/endian.h
qdcons.o: qdcons.c ../h/types.h ../vax/cpu.h ../ultrix/qdioctl.h
qdcons.o: ../ultrix/../h/ioctl.h ../ultrix/../h/ttychars.h
qdcons.o: ../ultrix/../h/ttydev.h ../ultrix/qevent.h ../ultrix/qduser.h
qdcons.o: ../ultrix/qevent.h ../ultrix/qdreg.h
qvcons.o: qvcons.c ../h/types.h ../ultrix/qvioctl.h ../ultrix/qevent.h
qvcons.o: ../ultrix/../h/ioctl.h ../ultrix/../h/ttychars.h
qvcons.o: ../ultrix/../h/ttydev.h ../ultrix/qvreg.h ../vax/cpu.h
qfont.o: qfont.c
rk.o: rk.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
rk.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
rk.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../h/disklabel.h
rk.o: ../vax/pte.h ../vaxuba/ubareg.h ../vaxuba/rkreg.h ../stand/saio.h
rk.o: ../stand/saioctl.h ../stand/saerrno.h savax.h
rl.o: rl.c ../machine/pte.h ../h/param.h /usr/include/sys/types.h ../h/signal.h
rl.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
rl.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../vaxuba/rlreg.h
rl.o: ../vaxuba/ubareg.h ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h
rl.o: savax.h
sys.o: ../stand/sys.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
sys.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
sys.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../h/dir.h
sys.o: ../h/reboot.h ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h
tm.o: tm.c ../machine/pte.h ../h/param.h /usr/include/sys/types.h ../h/signal.h
tm.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
tm.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../vaxuba/ubareg.h
tm.o: ../vaxuba/tmreg.h ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h
tm.o: savax.h
tmscp.o: tmscp.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
tmscp.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
tmscp.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../vax/pte.h
tmscp.o: savax.h ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h
tmscp.o: ../vaxuba/tmscpreg.h ../vaxuba/ubareg.h ../vax/tmscp.h
ts.o: ts.c ../machine/pte.h ../h/param.h /usr/include/sys/types.h ../h/signal.h
ts.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
ts.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../vaxuba/tsreg.h
ts.o: ../vaxuba/ubareg.h ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h
ts.o: savax.h
uba.o: uba.c ../machine/pte.h ../h/param.h /usr/include/sys/types.h
uba.o: ../h/signal.h /usr/include/machine/trap.h
uba.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
uba.o: ../h/inode.h ../h/vm.h ../h/vmparam.h ../h/../machine/vmparam.h
uba.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../h/fs.h ../vax/cpu.h
uba.o: ../vaxuba/ubareg.h ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h
uba.o: savax.h
uda.o: uda.c ../machine/pte.h ../h/param.h /usr/include/sys/types.h
uda.o: ../h/signal.h /usr/include/machine/trap.h
uda.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
uda.o: ../h/inode.h ../h/fs.h ../h/disklabel.h ../stand/saio.h
uda.o: ../stand/saioctl.h ../stand/saerrno.h savax.h ../vaxuba/udareg.h
uda.o: ../vaxuba/ubareg.h ../vax/mscp.h
up.o: up.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
up.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
up.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../h/dkbad.h
up.o: ../h/disklabel.h ../vax/pte.h ../vaxuba/upreg.h ../vaxuba/ubareg.h
up.o: ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h savax.h
upmaptype.o: upmaptype.c
ut.o: ut.c ../machine/pte.h ../h/param.h /usr/include/sys/types.h ../h/signal.h
ut.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
ut.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../vaxuba/ubareg.h
ut.o: ../vaxuba/utreg.h ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h
ut.o: savax.h
conf.o: conf.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
conf.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
conf.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../stand/saio.h
conf.o: ../stand/saioctl.h ../stand/saerrno.h
dkbad.o: ../vax/dkbad.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
dkbad.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
dkbad.o: /usr/include/machine/endian.h ../h/buf.h ../h/dkbad.h
confxx.o: confxx.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
