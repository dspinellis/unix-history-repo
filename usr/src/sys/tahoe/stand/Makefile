#	Makefile	7.3	86/11/04
#
DESTDIR=
LIBS=	libsa.a ${DESTDIR}/lib/libc.a
COPTS=	-O -I. -I../h
CFLAGS=	-DSTANDALONE -DTAHOE ${COPTS}
RELOC=	800
MTBOOTRELOC=e0000
BOOTRELOC=2e0000
SRCS=	sys.c conf.c prf.c machdep.c 
DRIVERS=udc.o cy.o vd.o
LDT1MBSYSOPT=-T ${MTBOOTRELOC} -x
LDTSYSOPT=-T ${BOOTRELOC} -x
LDTOPT=	-T ${RELOC} -e _entry -x
CFILES=	boot.c cat.c conf.c ls.c prf.c srt0.c sys.c ls.c \
	fastcopy.c devcopy.c cat.c  vd.c cy.c
SUBDIR=	vdformat
ALL=	boot boot1mb cat ls devcopy fastcopy

all:	srt0.o libsa.a ${ALL} ${SUBDIR}

install: all
	install -c -m 644 libsa.a ${DESTDIR}/usr/lib/libsa.a
	for i in ${ALL}; do \
		install -m 644 $$i ${DESTDIR}/stand; \
	done
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install) \
	done
	rm -f ${DESTDIR}/boot; cp ${DESTDIR}/stand/boot.fsd ${DESTDIR}/boot

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
	${CC} -E -DRELOC=0x${RELOC} ${CFLAGS} srt0.c | ${AS} -o srt0.o

bootsrt0.o: srt0.c
	${CC} -E -DRELOC=0x${BOOTRELOC} -DREL ${CFLAGS} srt0.c | \
	    ${AS} -o bootsrt0.o

boot1mbsrt0.o: srt0.c
	${CC} -E -DRELOC=0x${MTBOOTRELOC} -DREL ${CFLAGS} srt0.c | \
	    ${AS} -o boot1mbsrt0.o

sfrt0.o: sfrt0.c
	${CC} -E -DRELOC=0x${RELOC} -DREL ${CFLAGS} sfrt0.c | \
	    ${AS} -o sfrt0.o

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
ls.o: ls.c ../h/param.h /usr/include/machine/machparam.h ../h/signal.h
ls.o: /usr/include/sys/types.h ../h/inode.h ../h/dir.h ../h/fs.h ./saio.h
fastcopy.o: fastcopy.c
devcopy.o: devcopy.c
cat.o: cat.c
vd.o: vd.c ./../machine/mtpr.h ../h/param.h
vd.o: /usr/include/machine/machparam.h ../h/signal.h /usr/include/sys/types.h
vd.o: ../h/inode.h ../h/fs.h ./../tahoevba/vdreg.h ./../tahoevba/vbaparam.h
vd.o: ./saio.h
cy.o: cy.c ./../machine/pte.h ./../machine/mtpr.h ../h/param.h
cy.o: /usr/include/machine/machparam.h ../h/signal.h /usr/include/sys/types.h
cy.o: ../h/inode.h ../h/fs.h ./saio.h ./cyvar.h ./../tahoevba/vbaparam.h
