#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	7.8 (Berkeley) %G%
#
RELOC=		800
MTBOOTRELOC=	e0000
BOOTRELOC=	e0000
LDT1MBSYSOPT=	-T ${MTBOOTRELOC} -x
LDTSYSOPT=	-T ${BOOTRELOC} -x
LDTOPT=		-T ${RELOC} -e _entry -x

DESTDIR=
VPATH=	../stand
LIBS=	libsa.a ${DESTDIR}/lib/libc.a
COPTS=	-O -I. -I../stand -I../h
CFLAGS=	-DSTANDALONE -DCOMPAT_42 -DTAHOE -DRELOC=0x${RELOC} ${COPTS}
SUBDIR=	vdformat
ALL=	boot cat copy ls

SRCS=	boot.c cat.c conf.c copy.c dev.c getfile.c gets.c ls.c prf.c \
	srt0.c sys.c vd.c cy.c hdc.c
DRIVERS=cy.o vd.o hdc.o

#all: srt0.o libsa.a ${ALL} ${SUBDIR}
all: srt0.o libsa.a ${ALL}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

machdep.o: machdep.c ../tahoe/mtpr.h ../tahoe/mem.h ../tahoe/SYS.h
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

clean: FRC
	rm -f ${ALL} *.o *.map *.bak a.out ncy.c libsa.a
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done

depend: ${SRCS} FRC
	mkdep ${COPTS} ${SRCS}
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done

install: FRC
	-mkdir ${DESTDIR}/stand
	for i in ${ALL}; do \
		install -o bin -g bin -m 644 $$i ${DESTDIR}/stand/$$i; \
	done
	rm -f ${DESTDIR}/boot;
	cp ${DESTDIR}/stand/boot ${DESTDIR}/boot
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install) \
	done

tags: ${SRCS} FRC
	ctags ${SRCS}
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} tags); done

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

boot.o: boot.c ../machine/mtpr.h ../h/param.h /usr/include/sys/types.h
boot.o: ../h/signal.h /usr/include/machine/trap.h
boot.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
boot.o: ../h/inode.h ../h/fs.h ../h/vm.h /usr/include/sys/vmparam.h
boot.o: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
boot.o: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h ../stand/saio.h
boot.o: ../stand/saioctl.h ../stand/saerrno.h ../h/reboot.h
boot.o: /usr/include/a.out.h /usr/include/sys/exec.h
cat.o: ../stand/cat.c
conf.o: conf.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
conf.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
conf.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../stand/saio.h
conf.o: ../stand/saioctl.h ../stand/saerrno.h
copy.o: ../stand/copy.c
dev.o: ../stand/dev.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
dev.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
dev.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../stand/saio.h
dev.o: ../stand/saioctl.h ../stand/saerrno.h
getfile.o: ../stand/getfile.c ../h/param.h /usr/include/sys/types.h
getfile.o: ../h/signal.h /usr/include/machine/trap.h
getfile.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
getfile.o: ../h/inode.h ../h/fs.h ../stand/saio.h ../stand/saioctl.h
getfile.o: ../stand/saerrno.h
gets.o: ../stand/gets.c
ls.o: ../stand/ls.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
ls.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
ls.o: /usr/include/machine/endian.h ../h/inode.h ../h/dir.h ../h/fs.h
ls.o: ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h
prf.o: prf.c ../machine/mtpr.h ../h/param.h /usr/include/sys/types.h
prf.o: ../h/signal.h /usr/include/machine/trap.h
prf.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
prf.o: ../tahoe/cp.h
srt0.o: srt0.c ../machine/mtpr.h
sys.o: ../stand/sys.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
sys.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
sys.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../h/dir.h
sys.o: ../h/reboot.h ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h
vd.o: vd.c ../machine/mtpr.h ../h/param.h /usr/include/sys/types.h
vd.o: ../h/signal.h /usr/include/machine/trap.h
vd.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
vd.o: ../h/inode.h ../h/fs.h ../h/buf.h ../h/disklabel.h ../stand/saio.h
vd.o: ../stand/saioctl.h ../stand/saerrno.h ../tahoevba/vdreg.h
vd.o: ../tahoevba/vbaparam.h
cy.o: cy.c ../machine/pte.h ../machine/mtpr.h ../h/param.h
cy.o: /usr/include/sys/types.h ../h/signal.h /usr/include/machine/trap.h
cy.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
cy.o: ../h/inode.h ../h/fs.h ../stand/saio.h ../stand/saioctl.h
cy.o: ../stand/saerrno.h ../tahoevba/cyreg.h ../tahoevba/vbaparam.h
hdc.o: hdc.c /usr/include/machine/mtpr.h ../h/param.h ../h/types.h
hdc.o: ../h/signal.h /usr/include/machine/trap.h ../h/../machine/machparam.h
hdc.o: /usr/include/machine/endian.h ../h/inode.h ../h/fs.h ../h/buf.h
hdc.o: ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/disklabel.h
hdc.o: ../stand/saio.h ../stand/saioctl.h ../stand/saerrno.h ../tahoevba/hdc.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
