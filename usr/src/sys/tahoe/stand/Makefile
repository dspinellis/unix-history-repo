#	Makefile	7.7	87/11/03
#
DESTDIR=
LIBS=	libsa.a ${DESTDIR}/lib/libc.a
RELOC=	800
MTBOOTRELOC=e0000
BOOTRELOC=e0000
COPTS=	-O -I. -I../h
CFLAGS=	-DSTANDALONE -DTAHOE -DRELOC=0x${RELOC} ${COPTS}
SRCS=	sys.c conf.c prf.c machdep.c 
DRIVERS=udc.o cy.o vd.o hdc.o
LDT1MBSYSOPT=-T ${MTBOOTRELOC} -x
LDTSYSOPT=-T ${BOOTRELOC} -x
LDTOPT=	-T ${RELOC} -e _entry -x
CFILES=	boot.c cat.c conf.c ls.c prf.c srt0.c sys.c ls.c \
	fastcopy.c devcopy.c copy.c cat.c vd.c cy.c hdc.c
SUBDIR=	vdformat
ALL=	boot cat copy ls devcopy fastcopy

all:	srt0.o libsa.a ${ALL} ${SUBDIR}

install: all
	-mkdir ${DESTDIR}/stand
	for i in ${ALL}; do \
		install -m 644 $$i ${DESTDIR}/stand/$$i; \
	done
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install) \
	done
	rm -f ${DESTDIR}/boot; cp ${DESTDIR}/stand/boot ${DESTDIR}/boot

clean:
	rm -f ${ALL} *.o *.map *.bak a.out ncy.c libsa.a
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} clean) \
	done

machdep.o: machdep.c ../tahoe/mtpr.h ../tahoe/mem.h ../tahoe/SYS.h
	${CC} -E ${CFLAGS} machdep.c | ${AS}  -o machdep.o

libsa.a: sys.o conf.o ${DRIVERS} prf.o machdep.o 
	ar crv $@ $?
	ranlib $@

boot:	boot.o bootsrt0.o ${LIBS}
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

cat:	cat.o srt0.o ${LIBS}
	ld ${LDTOPT} -o cat -s srt0.o cat.o ${LIBS}

devcopy: devcopy.o srt0.o ${LIBS}
	ld ${LDTOPT} -o devcopy -s srt0.o devcopy.o ${LIBS}

copy:	copy.o srt0.o ncy.o ${LIBS}
	ld ${LDTOPT} -o copy srt0.o copy.o ncy.o ${LIBS}

fastcopy: fastcopy.o srt0.o ncy.o ${LIBS}
	ld ${LDTOPT} -o fastcopy -s srt0.o fastcopy.o ncy.o ${LIBS}

xpformat: xpformat.o srt0.o ${LIBS}
	ld ${LDTOPT} -o xpformat srt0.o xpformat.o ${LIBS}

vdformat: FRC
	cd vdformat; make ${MFLAGS}

FRC:

ls:	ls.o srt0.o ${LIBS}
	ld ${LDTOPT} -o ls -s srt0.o ls.o ${LIBS}

srt0.o: srt0.c
	${CC} -E ${CFLAGS} srt0.c | ${AS} -o srt0.o

bootsrt0.o: srt0.c
	${CC} -E -DBOOTRELOC=0x${BOOTRELOC} -DREL ${CFLAGS} srt0.c | \
	    ${AS} -o bootsrt0.o

boot1mbsrt0.o: srt0.c
	${CC} -E -DBOOTRELOC=0x${MTBOOTRELOC} -DREL ${CFLAGS} srt0.c | \
	    ${AS} -o boot1mbsrt0.o

ncy.c:	cy.c
	rm -f ncy.c
	ln cy.c ncy.c

ncy.o:	ncy.c
	${CC} -DNOBLOCK ${CFLAGS} -c ncy.c

print:
	@pr -f makefile
	@ls -l | pr -f
	@pr -f *.h *.c *.lst

tags: FRC
	ctags ${CFILES}

depend: FRC
	mkdep ${COPTS} ${CFILES}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

boot.o: boot.c ../machine/mtpr.h ../h/param.h /usr/include/sys/types.h
boot.o: ../h/signal.h /usr/include/machine/trap.h
boot.o: /usr/include/machine/machparam.h ../h/inode.h ../h/fs.h ../h/vm.h
boot.o: /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
boot.o: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
boot.o: /usr/include/sys/vmsystm.h saio.h ../h/reboot.h /usr/include/a.out.h
boot.o: /usr/include/sys/exec.h
cat.o: cat.c
conf.o: conf.c ../machine/pte.h ../h/param.h /usr/include/sys/types.h
conf.o: ../h/signal.h /usr/include/machine/trap.h
conf.o: /usr/include/machine/machparam.h ../h/inode.h ../h/fs.h saio.h
ls.o: ls.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
ls.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h ../h/inode.h
ls.o: ../h/dir.h ../h/fs.h saio.h
prf.o: prf.c ../machine/mtpr.h ../h/param.h /usr/include/sys/types.h
prf.o: ../h/signal.h /usr/include/machine/trap.h
prf.o: /usr/include/machine/machparam.h ../tahoe/cp.h
srt0.o: srt0.c ../machine/mtpr.h
sys.o: sys.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
sys.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
sys.o: ../h/inode.h ../h/fs.h ../h/dir.h ../h/reboot.h saio.h ../machine/mtpr.h
ls.o: ls.c ../h/param.h /usr/include/sys/types.h ../h/signal.h
ls.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h ../h/inode.h
ls.o: ../h/dir.h ../h/fs.h saio.h
fastcopy.o: fastcopy.c
devcopy.o: devcopy.c
copy.o: copy.c
cat.o: cat.c
vd.o: vd.c ../machine/mtpr.h ../h/param.h /usr/include/sys/types.h
vd.o: ../h/signal.h /usr/include/machine/trap.h
vd.o: /usr/include/machine/machparam.h ../h/inode.h ../h/fs.h ../h/buf.h
vd.o: ../h/disklabel.h saio.h ../tahoevba/vdreg.h ../tahoevba/vbaparam.h
cy.o: cy.c ../machine/pte.h ../machine/mtpr.h ../h/param.h
cy.o: /usr/include/sys/types.h ../h/signal.h /usr/include/machine/trap.h
cy.o: /usr/include/machine/machparam.h ../h/inode.h ../h/fs.h saio.h
cy.o: ../tahoevba/cyreg.h ../tahoevba/vbaparam.h
hdc.o: hdc.c /usr/include/machine/mtpr.h ../h/param.h ../h/types.h
hdc.o: ../h/signal.h /usr/include/machine/trap.h ../h/../machine/machparam.h
hdc.o: ../h/inode.h ../h/fs.h ../h/buf.h ../h/ioctl.h ../h/ttychars.h
hdc.o: ../h/ttydev.h ../h/disklabel.h saio.h ../tahoevba/dsk.h
hdc.o: ../tahoevba/dskio.h ../tahoevba/hdc.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
