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
#	@(#)Makefile	7.2 (Berkeley) %G%
#

DESTDIR=/
INCPATH=-I. -I../../sys -I../../  -I../../stand
CC=cc -traditional
CPP= cpp -traditional ${INCPATH} -DSTANDALONE -DAT386
CFLAGS=	${COPTS} 
COPTS=	-O ${INCPATH} -DSTANDALONE -DAT386
RELOC=	70000
RELOC2=	70200

SRCS=	boot.c bootxx.c copy.c \
	sys.c conf.c prf.c  kbd.c cga.c \
	wd.c fd.c gets.c srt0.c \
	wdbootblk.c fdbootblk.c
DUMMIES= confxx.c
DRIVERS= kbd.o cga.o wd.o fd.o
LIBSA=	libsa.a

BOOTBLOCKS= wdboot bootwd fdboot bootfd
DISKUTILS= boot copy cat ls mem
ALL=	${BOOTBLOCKS} ${DISKUTILS}

STANDDIR= ${DESTDIR}/stand
all: ${ALL}

${LIBSA}: sys.o conf.o ${DRIVERS} prf.o gets.o getfile.o
	ar crv ${LIBSA} $?
	ranlib ${LIBSA}

${DRIVERS}:
	${CC} -c ${COPTS} $*.c

# startups

srt0.o: srt0.c
	${CPP} -E -DLOCORE -DRELOC=0x${RELOC} srt0.c | ${AS} -o srt0.o

wsrt0.o: srt0.c
	${CPP} -E -DLOCORE -DSMALL -DRELOC=0x${RELOC} -DREL srt0.c | ${AS} -o wsrt0.o

relsrt0.o: srt0.c
	${CPP} -E -DLOCORE -DRELOC=0x${RELOC} -DREL srt0.c | \
	${AS} -o relsrt0.o

# block 0 boots

wdbootblk.o: wdbootblk.c 
	${CPP} -E -DLOCORE -DRELOC=0x${RELOC} wdbootblk.c | \
	as -o wdbootblk.o

fdbootblk.o: fdbootblk.c 
	${CPP} -E -DLOCORE -DRELOC=0x${RELOC} fdbootblk.c | \
	as -o fdbootblk.o

# bootable from floppy or real disks

boot:	boot.o relsrt0.o bootconf.o ${LIBSA}
	ld -N -T ${RELOC} -o boot relsrt0.o boot.o bootconf.o ${LIBSA} -lc

bootconf.o: conf.o
	cp conf.c bootconf.c
	${CC} -c ${CFLAGS} -DBOOT bootconf.c
	rm -f bootconf.c

cat:	cat.o srt0.o conf.o ${LIBSA}
	ld -N -o cat srt0.o cat.o conf.o ${LIBSA} -lc

ls:	ls.o srt0.o conf.o ${LIBSA}
	ld -N -o ls srt0.o ls.o conf.o ${LIBSA} -lc

copy:	copy.o srt0.o conf.o ${LIBSA}
	ld -N -o copy srt0.o copy.o conf.o ${LIBSA} -lc

mem: mem.o prf.o kbd.o cga.o srt0.o gets.o
	ld -N -o mem srt0.o mem.o kbd.o cga.o prf.o gets.o
	
# getting booted from disc

wdboot: wdbootblk.o
	ld -N -T ${RELOC} wdbootblk.o
	cp a.out wdb
	rm -f wdboot
	strip a.out;dd if=a.out of=wdboot ibs=32 skip=1;rm -f a.out
	ls -l wdboot

bootwd: wdbootblk.o wsrt0.o bootxx.o confwd.o sm_wd.o sm_sys.o sm_cga.o ${LIBSA}
	ld -N -T ${RELOC2} wsrt0.o bootxx.o confwd.o sm_sys.o \
		sm_wd.o sm_cga.o ${LIBSA} -lc
	size a.out
	cp a.out bwd
	rm -f bootwd
	strip a.out;dd if=a.out of=bootwd ibs=32 skip=1;rm -f a.out
	ls -l bootwd

fdboot: fdbootblk.o
	ld -N -T ${RELOC} fdbootblk.o
	cp a.out fdb
	rm -f fdboot
	strip a.out;dd if=a.out of=fdboot ibs=32 skip=1;rm -f a.out
	ls -l fdboot

bootfd: fdbootblk.o wsrt0.o bootxx.o conffd.o fd.o sm_sys.o sm_cga.o ${LIBSA}
	ld -N -T ${RELOC2} wsrt0.o bootxx.o conffd.o sm_sys.o \
		fd.o sm_cga.o ${LIBSA} -lc
	size a.out
	cp a.out bfd
	rm -f bootfd
	strip a.out; dd if=a.out of=bootfd ibs=32 skip=1;rm -f a.out
	ls -l bootfd

sm_wd.o: wd.o
	-rm -f sm_wd.c
	-ln -s wd.c sm_wd.c
	${CC} -S -DSMALL ${CFLAGS} sm_wd.c
	${AS} sm_wd.s -o sm_wd.o
	rm -f sm_wd.s sm_wd.c

sm_cga.o: cga.o
	-rm -f sm_cga.c
	-ln -s cga.c sm_cga.c
	${CC} -S -DSMALL ${CFLAGS} sm_cga.c
	${AS} sm_cga.s -o sm_cga.o
	rm -f sm_cga.s sm_cga.c

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

sm_sys.o: sys.o
	-rm -f sm_sys.c
	-ln -s sys.c sm_sys.c
	${CC} -c -DSMALL ${CFLAGS} sm_sys.c
	-rm -f sm_sys.c

confwd.o: confwd.c confxx.o
conffd.o: conffd.c confxx.o

confxx.o:
	touch confxx.o

confwd.c: confxx.c
	-rm -f confwd.c
	sed -e 's/XX/wd/' -e 's/xx/wd/g' <confxx.c >confwd.c

conffd.c: confxx.c
	-rm -f conffd.c
	sed -e 's/XX/fd/' -e 's/xx/fd/g' <confxx.c >conffd.c

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

