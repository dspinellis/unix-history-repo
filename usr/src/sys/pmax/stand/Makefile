#	@(#)Makefile	5.3 (Berkeley) %G%

DESTDIR=
STAND=	../../stand
INCPATH=-I. -I../..
VPATH=	${STAND}

# RELOC=80200000 allows for boot prog up to 1D0000 (1900544) bytes long
RELOC=	80200000

DEFS= -DSTANDALONE -DDS5000 -DSMALL
CFLAGS=	-O ${INCPATH} ${DEFS}
AFLAGS=	-O ${INCPATH} ${DEFS} -DLOCORE

DRIVERS=asc.c rz.c sii.c scsi.c
SRCS=	conf.c machdep.c trap.c ${DRIVERS}
LIBS=	libdrive.a libsa/libsa.a ../../libkern/obj/libkern.a

#ALL=	boot mkboot mkboottape
ALL=	boot

all: ${ALL}

boot: ${LIBS}

libsa/libsa.a::
	cd libsa; make

libdrive.a: conf.o machdep.o trap.o ${DRIVERS:.c=.o}
	ar crv $@ $?
	ranlib $@

${DRIVERS}: samachdep.h

# depend on DEFS

machdep.o rz.o trap.o: Makefile
cons.o dca.o hil.o: Makefile
ite.o ite_subr.o ite_dv.o ite_gb.o ite_hy.o ite_rb.o ite_tc.o: Makefile

# bootable from real disks

boot:	locore.o boot.o bootconf.o ${LIBS}
	ld -N -T ${RELOC} -e start locore.o boot.o bootconf.o ${LIBS} -o $@ 

bootconf.o: conf.o
	rm -f bootconf.c
	ln -s conf.c bootconf.c
	${CC} -c ${CFLAGS} -DBOOT bootconf.c
	rm -f bootconf.c

mkboot: mkboot.c
	${CC} ${CFLAGS} -o mkboot mkboot.c

mkboottape: mkboottape.c
	${CC} ${CFLAGS} -o mkboottape mkboottape.c

# utilities

clean cleandir:
	rm -f .depend *.o *.exe *.i errs make.out core*
	rm -f a.out boot cat
	rm -f boot[a-z][a-z] [a-z][a-z]boot
	rm -f libdrive.a mkboot mkboottape
	cd libsa; make cleandir

install:
	./mkboot boot rzboot bootrz
	install -s -o bin -g bin -m 444 rzboot ${DESTDIR}/usr/mdec
	install -s -o bin -g bin -m 444 bootrz ${DESTDIR}/usr/mdec

depend: ${SRCS}
	mkdep ${INCPATH} ${DEFS} ${SRCS}
	mkdep -a -p ${INCPATH} ${DEFS} mkboot.c mkboottape.c
	cd libsa; make depend
