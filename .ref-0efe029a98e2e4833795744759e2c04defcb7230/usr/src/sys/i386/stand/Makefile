#-
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# William Jolitz.
#
#
# Copying or redistribution in any form is explicitly forbidden
# unless prior written permission is obtained from William Jolitz or an
# authorized representative of the University of California, Berkeley.
#
# Freely redistributable copies of this code will be available in
# the near future; for more information contact William Jolitz or
# the Computer Systems Research Group at the University of California,
# Berkeley.
#
# The name of the University may not be used to endorse or promote
# products derived from this software without specific prior written
# permission.  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE.
#	@(#)Makefile	7.1 (Berkeley) %G%
#
DESTDIR=/
INCPATH=-I. -I../h -I../AT386
CC=gcc
CFLAGS=	${COPTS} 
COPTS=	-O ${INCPATH} -DSTANDALONE -DAT386
RELOC=	70000

SRCS=	boot.c bootxx.c copy.c \
	sys.c conf.c prf.c  kbdcga.c \
	wd.c gets.c srt0.c \
	wdboot.c
DUMMIES= confxx.c
DRIVERS= kbd.o cga.o wd.o disktab.o
LIBSA=	libsa.a

BOOTBLOCKS= bootwd
DISKUTILS= boot copy cat ls
ALL=	${BOOTBLOCKS} ${DISKUTILS}

STANDDIR= ${DESTDIR}/stand
all: ${ALL}

${LIBSA}: sys.o conf.o ${DRIVERS} prf.o gets.o
	ar crv ${LIBSA} $?
	ranlib ${LIBSA}

${DRIVERS}: # saio.h
	${CC} -c ${COPTS} $*.c

test: test.o prf.o kbd.o cga.o srt0.o gets.o
	ld -N -o test srt0.o test.o kbd.o cga.o prf.o gets.o
	
t: test.o prf.o kbd.o cga.o relsrt0.o gets.o
	ld -N -T ${RELOC} -o t relsrt0.o test.o kbd.o cga.o prf.o gets.o
	
mem: mem.o prf.o kbd.o cga.o srt0.o gets.o
	ld -N -o mem srt0.o mem.o kbd.o cga.o prf.o gets.o
	
# startups

srt0.o: srt0.c
	${CC} -E -DLOCORE -DRELOC=0x${RELOC} ${COPTS} srt0.c | a386 -o srt0.o

relsrt0.o: srt0.c
	${CC} -E -DLOCORE -DRELOC=0x${RELOC} -DREL ${COPTS} srt0.c | \
	a386 -o relsrt0.o

wdboot.o: wdboot.c ../h/disk.h ../32k/psr.h ../S375/devvm.h
	${CC} -E -DLOCORE -DRELOC=0x${RELOC} ${COPTS} wdboot.c | as -o wdboot.o

dtab.o: dtab.c ../h/disk.h
	${CC} -E -DLOCORE -DRELOC=0x${RELOC} -DDISKLAB ${COPTS} dtab.c | \
	as -o dtab.o


# bootable from tape

tpboot:	tpboot.o relsrt0.o ${LIBSA}
	ld -N -o tpboot -T ${RELOC} relsrt0.o tpboot.o ${LIBSA} -lc3
	strip tpboot

tpboot.o: boot.c ../h/param.h ../h/inode.h ../h/fs.h
tpboot.o: saio.h ../h/reboot.h ../h/vm.h 
	cp boot.c tpboot.c; chmod +w tpboot.c
	${CC} -c ${CFLAGS} -DJUSTASK tpboot.c
	rm -f tpboot.c

tpcopy:	copy.o tpsrt0.o ${LIBSA}
	ld -N -o tpcopy tpsrt0.o copy.o ${LIBSA} -lc3
	strip tpcopy

tpformat: format.o wdformat.o tpsrt0.o ${LIBSA}
	cp format.c tpformat.c; chmod +w tpformat.c
	${CC} -c ${CFLAGS} -DJUSTEXIT -DTP tpformat.c
	rm -f tpformat.c
	ld -N -o tpformat tpsrt0.o tpformat.o wdformat.o ${LIBSA} -lc3
	strip tpformat

tpdisklabel:	disklabel.o tpsrt0.o ${LIBSA}
	cp disklabel.c tpdisklabel.c; chmod +w tpdisklabel.c
	${CC} -c ${CFLAGS} -DJUSTEXIT -DTP tpdisklabel.c
	rm -f tpdisklabel.c
	ld -N -o tpdisklabel tpsrt0.o tpdisklabel.o ${LIBSA} -lc3

# bootable from floppy or real disks

boot:	boot.o relsrt0.o bootconf.o ${LIBSA}
	ld -N -T ${RELOC} -o boot relsrt0.o boot.o bootconf.o ${LIBSA} -lc3

dlbl:	dlbl.o srt0.o bootconf.o ${LIBSA}
	ld -N -o dlbl srt0.o dlbl.o bootconf.o ${LIBSA} -lc3

bootconf.o: conf.o
	cp conf.c bootconf.c
	${CC} -c ${CFLAGS} -DBOOT bootconf.c
	rm -f bootconf.c

cat:	cat.o srt0.o conf.o ${LIBSA}
	ld -N -o cat srt0.o cat.o conf.o ${LIBSA} -lc3

ls:	ls.o srt0.o conf.o ${LIBSA}
	ld -N -o ls srt0.o ls.o conf.o ${LIBSA} -lc3

copy:	copy.o srt0.o conf.o ${LIBSA}
	ld -N -o copy srt0.o copy.o conf.o ${LIBSA} -lc3

format:	format.o wdformat.o srt0.o conf.o ${LIBSA}
	ld -N -o format srt0.o format.o wdformat.o conf.o ${LIBSA} -lc3

disklabel:	disklabel.o srt0.o conf.o ${LIBSA}
	ld -N -o disklabel srt0.o disklabel.o conf.o ${LIBSA} -lc3

icheck:	icheck.o srt0.o conf.o ${LIBSA}
	ld -N -o icheck srt0.o icheck.o conf.o ${LIBSA} -lc3

# bootstrap from ether

### not yet, rosin, not yet ###

# getting booted from disc

bootwd: wdboot.o bootxx.o confwd.o sm_wd.o sm_sys.o ${LIBSA}
	ld -N -T ${RELOC} wdboot.o bootxx.o confwd.o sm_sys.o sm_wd.o \
		${LIBSA} -lc3
	size a.out
	strip a.out;dd if=a.out of=bootwd ibs=32 skip=1;rm -f a.out
	ls -l bootwd

bootcst: tprelsrt0.o bootxx.o confcst.o sm_cst.o sm_sba.o sm_sys.o ${LIBSA}
	ld -N -T ${RELOC} tprelsrt0.o bootxx.o confcst.o sm_sys.o \
		sm_cst.o sm_sba.o ${LIBSA} -lc3
	size a.out
	strip a.out;dd if=a.out of=bootcst ibs=32 skip=1;rm -f a.out
	ls -l bootcst

bootswd: relsrt0.o dtab.o bootxx.o confswd.o sm_swd.o sm_sba.o sm_sys.o ${LIBSA}
	ld -N -T ${RELOC} relsrt0.o sm_swd.o dtab.o bootxx.o confswd.o sm_sba.o \
		sm_sys.o ${LIBSA} -lc3
	size a.out
	strip a.out;dd if=a.out of=bootswd ibs=32 skip=1;rm -f a.out
	ls -l bootswd

sm_wd.o: wd.o
	-rm -f sm_wd.c
	-ln -s wd.c sm_wd.c
	${CC} -S -DSMALL ${CFLAGS} sm_wd.c
	/lib/c2 -i sm_wd.s | as -o sm_wd.o
	rm -f sm_wd.s sm_wd.c

sm_cst.o: cst.o
	-rm -f sm_cst.c
	-ln -s cst.c sm_cst.c
	${CC} -S -DSMALL ${CFLAGS} sm_cst.c
	/lib/c2 -i sm_cst.s | as -o sm_cst.o
	rm -f sm_cst.s sm_cst.c

sm_swd.o: swd.o
	-rm -f sm_swd.c
	-ln -s swd.c sm_swd.c
	${CC} -S -DSMALL ${CFLAGS} sm_swd.c
	/lib/c2 -i sm_swd.s | as -o sm_swd.o
	rm -f sm_swd.s sm_swd.c

sm_sba.o: sba.o
	-rm -f sm_sba.c
	-ln -s sba.c sm_sba.c
	${CC} -S -DSMALL ${CFLAGS} sm_sba.c
	/lib/c2 -i sm_sba.s | as -o sm_sba.o
	rm -f sm_sba.s sm_sba.c

sm_sys.o: sys.o
	-rm -f sm_sys.c
	-ln -s sys.c sm_sys.c
	${CC} -c -DSMALL ${CFLAGS} sm_sys.c
	-rm -f sm_sys.c

confwd.o: confwd.c confxx.o
confswd.o: confswd.c confxx.o
confcst.o: confcst.c confxx.o

confxx.o:
	touch confxx.o

confwd.c: confxx.c
	-rm -f confwd.c
	sed -e 's/XX/wd/' -e 's/xx/wd/g' <confxx.c >confwd.c

confswd.c: confxx.c
	-rm -f confswd.c
	sed -e 's/XX/swd/' -e 's/xx/swd/g' <confxx.c >confswd.c

confcst.c: confxx.c
	-rm -f confcst.c
	echo "#define	TP" >confcst.c
	sed -e 's/XX/cst/' -e 's/xx/cst/g' <confxx.c >>confcst.c

# utilities

print:
	@pr makefile
	@ls -l | pr 
	@pr *.h *.c

clean:
	rm -f *.o *.exe *.i errs sm_*.c
	rm -f a.out boot cat tpboot tpcopy copy tpformat disklabel \
		tpdisklabel ls
	rm -f boot[a-wyz]? boot[a-wyz]?? boot[a-wyz]?.c boot[a-wyz]??.c \
		conf[a-wyz]?.c conf[a-wyz]??.c
	rm -f format core sboot bootconf.c

lint:
	lint ${COPTS} -hxbn boot.c ${SRCS} | \
	    grep -v 'possible pointer alignment' | \
	    grep -v 'struct/union .* never defined'

install: ${ALL}
	install -c -s boot ${DESTDIR}
	cp ${DISKUTILS} ../dist/floppy
	cp ${TAPEUTILS} ../dist/tp
	cp ${BOOTBLOCKS} ${STANDDIR}
	cp ${DISKUTILS} ${STANDDIR}

depend:
	for i in ${SRCS} ${DUMMIES}; do \
	    ${CC} -M ${INCPATH} $$i | \
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

conf.o: conf.c ./../h/param.h
conf.o: /usr/include/machine/machparam.h ../h/signal.h /usr/include/sys/types.h
conf.o: ./../h/inode.h ./../h/fs.h ./saio.h

prf.o: prf.c
prf.o: /usr/include/sys/types.h

format.o: format.c ./../h/param.h /usr/include/machine/machparam.h
format.o: ../h/signal.h /usr/include/sys/types.h ./../h/fs.h ./../h/inode.h
format.o: ./../h/dkbad.h ./saio.h

bootxx.o: bootxx.c ./../h/param.h /usr/include/machine/machparam.h
bootxx.o: ../h/signal.h /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h
bootxx.o: /usr/include/sys/exec.h ./saio.h ./../h/reboot.h

confxx.o: confxx.c ./../machine/pte.h ./../h/param.h
confxx.o: /usr/include/machine/machparam.h ../h/signal.h
confxx.o: /usr/include/sys/types.h ./../h/inode.h ./../h/fs.h ./saio.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
