#	Makefile	7.2	86/01/21
#
DESTDIR=
LIBS=	libsa.a ${DESTDIR}/lib/libc.a
COPTS=	-O -I. -I../h
CFLAGS=	-DSTANDALONE -DTAHOE ${COPTS}
RELOC=	70000
MTARELOC=98000
BOOTRELOC=100000
SRCS=	sys.c conf.c prf.c machdep.c 
DRIVERS=udc.o cy.o vddc.o
LDT1MBSYSOPT=-T ${MTARELOC} -x
LDTSYSOPT=-T ${BOOTRELOC} -x
LDTOPT=	-T ${RELOC} -e _entry -x
LDTSF=	-N -x
CFILES=	boot.c cat.c conf.c ls.c prf.c srt0.c sys.c vdformat.c ls.c \
	fastcopy.c devcopy.c cat.c  vddc.c cy.c
ALL=	boot.fsd boot.smd boot.xfd boot.xsd \
	boot1mb.fsd boot1mb.smd boot1mb.xfd boot1mb.xsd \
	cat ls devcopy fastcopy vdformat

all:	srt0.o libsa.a ${ALL}

install: all
	install -c -m 644 libsa.a ${DESTDIR}/usr/lib/libsa.a
	for i in ${ALL}; do install -m 644 $$i ${DESTDIR}/stand; done
	rm -f ${DESTDIR}/boot; cp ${DESTDIR}/stand/boot.fsd ${DESTDIR}/boot

clean:
	rm -f ${ALL} *.o *.map *.bak a.out ncy.c libsa.a

machdep.o: machdep.c ../tahoe/mtpr.h ../tahoe/mem.h ../tahoe/SYS.h
	${CC} -E ${CFLAGS} machdep.c | ${AS}  -o machdep.o

libsa.a: sys.o conf.o ${DRIVERS} prf.o machdep.o 
	ar crv $@ $?
	ranlib $@

boot.fsd: boot.fsd.o bootsrt0.o ${LIBS}
	ld ${LDTSYSOPT} bootsrt0.o boot.fsd.o ${LIBS}
	dd if=a.out of=boot.fsd ibs=1024 skip=1

boot.smd: boot.smd.o bootsrt0.o ${LIBS}
	ld ${LDTSYSOPT} bootsrt0.o boot.smd.o ${LIBS}
	dd if=a.out of=boot.smd ibs=1024 skip=1

boot.xfd: boot.xfd.o bootsrt0.o ${LIBS}
	ld ${LDTSYSOPT} bootsrt0.o boot.xfd.o ${LIBS}
	dd if=a.out of=boot.xfd ibs=1024 skip=1

boot.xsd: boot.xsd.o bootsrt0.o ${LIBS}
	ld ${LDTSYSOPT} bootsrt0.o boot.xsd.o ${LIBS}
	dd if=a.out of=boot.xsd ibs=1024 skip=1

boot1mb.fsd: boot.fsd.o boot1mbsrt0.o ${LIBS}
	ld ${LDT1MBSYSOPT} boot1mbsrt0.o boot.fsd.o ${LIBS}
	dd if=a.out of=boot1mb.fsd ibs=1024 skip=1

boot1mb.smd: boot.smd.o boot1mbsrt0.o ${LIBS}
	ld ${LDT1MBSYSOPT} boot1mbsrt0.o boot.smd.o ${LIBS}
	dd if=a.out of=boot1mb.smd ibs=1024 skip=1

boot1mb.xfd: boot.xfd.o boot1mbsrt0.o ${LIBS}
	ld ${LDT1MBSYSOPT} boot1mbsrt0.o boot.xfd.o ${LIBS}
	dd if=a.out of=boot1mb.xfd ibs=1024 skip=1

boot1mb.xsd: boot.xsd.o boot1mbsrt0.o ${LIBS}
	ld ${LDT1MBSYSOPT} boot1mbsrt0.o boot.xsd.o ${LIBS}
	dd if=a.out of=boot1mb.xsd ibs=1024 skip=1

cat:	cat.o srt0.o ${LIBS}
	ld ${LDTOPT} -o cat -s srt0.o cat.o ${LIBS}

devcopy: devcopy.o srt0.o ${LIBS}
	ld ${LDTOPT} -o devcopy -s srt0.o devcopy.o ${LIBS}

fastcopy: fastcopy.o srt0.o ncy.o ${LIBS}
	ld ${LDTOPT} -o fastcopy -s srt0.o fastcopy.o ncy.o ${LIBS}

xpformat: xpformat.o srt0.o ${LIBS}
	ld ${LDTOPT} -o xpformat srt0.o xpformat.o ${LIBS}

vdformat: vdformat.o srt0.o ${LIBS}
	ld ${LDTOPT} -o vdformat -s srt0.o vdformat.o ${LIBS}

ls:	ls.o srt0.o ${LIBS}
	ld ${LDTOPT} -o ls -s srt0.o ls.o ${LIBS}

boot.fsd.o: boot.c
	${CC} -c $(COPTS) -DFSD boot.c
	mv boot.o $@

boot.smd.o: boot.c
	${CC} -c $(COPTS) -DSMD boot.c
	mv boot.o $@

boot.xfd.o: boot.c
	${CC} -c $(COPTS) -DXFD boot.c
	mv boot.o $@

boot.xsd.o: boot.c
	${CC} -c $(COPTS) -DXFD boot.c
	mv boot.o $@

srt0.o: srt0.c
	${CC} -E -DRELOC=0x${RELOC} ${CFLAGS} srt0.c | ${AS} -o srt0.o

bootsrt0.o: srt0.c
	${CC} -E -DRELOC=0x${BOOTRELOC} -DREL ${CFLAGS} srt0.c | ${AS} -o bootsrt0.o

boot1mbsrt0.o: srt0.c
	${CC} -E -DRELOC=0x${MTARELOC} -DREL ${CFLAGS} srt0.c | ${AS} -o boot1mbsrt0.o

sfrt0.o: sfrt0.c
	${CC} -E -DRELOC=0x${RELOC} -DREL ${CFLAGS} sfrt0.c | ${AS} -o sfrt0.o

ncy.c:	cy.c
	ln cy.c ncy.c

ncy.o:	ncy.c
	${CC} -DNOBLOCK ${CFLAGS} -c ncy.c

print:
	@pr -f makefile
	@/usr/ucb/ls -l | pr -f
	@pr -f *.h *.c *.lst

depend:
	${CC} -M ${COPTS} ${CFILES} | \
	sed -e ':loop' \
	    -e 's/\.\.\/[^ /]*\/\.\./../' \
	    -e 't loop' | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ex - Makefile < eddep
	rm eddep makedep

# DO NOT DELETE THIS LINE -- make depend uses it

boot.o: boot.c ./../machine/mtpr.h ../h/param.h
boot.o: /usr/include/machine/machparam.h ../h/signal.h /usr/include/sys/types.h
boot.o: ../h/inode.h ../h/fs.h ../h/vm.h /usr/include/sys/vmparam.h
boot.o: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
boot.o: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h ./saio.h
boot.o: ../h/reboot.h /usr/include/a.out.h /usr/include/sys/exec.h
cat.o: cat.c
conf.o: conf.c ./../machine/pte.h ../h/param.h /usr/include/machine/machparam.h
conf.o: ../h/signal.h /usr/include/sys/types.h ../h/inode.h ../h/fs.h ./saio.h
ls.o: ls.c ../h/param.h /usr/include/machine/machparam.h ../h/signal.h
ls.o: /usr/include/sys/types.h ../h/inode.h ../h/dir.h ../h/fs.h ./saio.h
prf.o: prf.c ./../machine/mtpr.h ../h/param.h /usr/include/machine/machparam.h
prf.o: ../h/signal.h /usr/include/sys/types.h ./../tahoe/cp.h
srt0.o: srt0.c ./../machine/mtpr.h
sys.o: sys.c ./../machine/mtpr.h ../h/param.h /usr/include/machine/machparam.h
sys.o: ../h/signal.h /usr/include/sys/types.h ../h/inode.h ../h/fs.h ../h/dir.h
sys.o: ./saio.h
vdformat.o: vdformat.c ./../machine/mtpr.h ../h/param.h
vdformat.o: /usr/include/machine/machparam.h ../h/signal.h
vdformat.o: /usr/include/sys/types.h ../h/inode.h ../h/fs.h ./saio.h
vdformat.o: ./../tahoevba/vbaparam.h ./../tahoevba/vdreg.h
vdformat.o: /usr/include/setjmp.h
ls.o: ls.c ../h/param.h /usr/include/machine/machparam.h ../h/signal.h
ls.o: /usr/include/sys/types.h ../h/inode.h ../h/dir.h ../h/fs.h ./saio.h
fastcopy.o: fastcopy.c
devcopy.o: devcopy.c
cat.o: cat.c
vddc.o: vddc.c ./../machine/mtpr.h ../h/param.h
vddc.o: /usr/include/machine/machparam.h ../h/signal.h /usr/include/sys/types.h
vddc.o: ../h/inode.h ../h/fs.h ./../tahoevba/vdreg.h ./../tahoevba/vbaparam.h
vddc.o: ./saio.h
cy.o: cy.c ./../machine/pte.h ./../machine/mtpr.h ../h/param.h
cy.o: /usr/include/machine/machparam.h ../h/signal.h /usr/include/sys/types.h
cy.o: ../h/inode.h ../h/fs.h ./saio.h ./cyvar.h ./../tahoevba/vbaparam.h
