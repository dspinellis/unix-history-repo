#	@(#)Makefile	8.3 (Berkeley) %G%

DESTDIR=
STAND=	../../stand
INCPATH=-I. -I/sys
VPATH=	${STAND}

# RELOC=80200000 allows for boot prog up to 1D0000 (1900544) bytes long
RELOC=	80200000

DEFS= -DSTANDALONE -DSMALL
CFLAGS=	-O2 ${INCPATH} ${DEFS}
AFLAGS=	-O2 ${INCPATH} ${DEFS} -DLOCORE

DRIVERS= rz.c
SRCS=	${DRIVERS}
LIBS=	libdrive.a libsa/libsa.a ../../libkern/obj/libkern.a

ALL=	boot mkboot mkboottape dec_label

.s.o:
	${CPP} -E ${CFLAGS:M-[ID]*} ${AINC} ${.IMPSRC} | \
	    ${AS} -o ${.TARGET}

all: ${ALL}

boot: ${LIBS}

libsa/libsa.a::
	cd libsa; make

libdrive.a: conf.o ${DRIVERS:.c=.o}
	ar crv $@ $?
	ranlib $@

# depend on DEFS

# bootable from real disks

boot:	start.o boot.o bootconf.o ${LIBS}
	ld -N -Ttext ${RELOC} -e start start.o boot.o bootconf.o ${LIBS} -o $@ 

start.o: start.s
	${CPP} -E ${CFLAGS:M-[ID]*} -DLOCORE ${AINC} ${.IMPSRC} | \
	    ${AS} -o ${.TARGET}

bootconf.o: conf.o
	rm -f bootconf.c
	ln -s conf.c bootconf.c
	${CC} -c ${CFLAGS} -DBOOT bootconf.c
	rm -f bootconf.c

mkboot: mkboot.c
	${CC} ${CFLAGS} -o mkboot mkboot.c

mkboottape: mkboottape.c
	${CC} ${CFLAGS} -o mkboottape mkboottape.c

dec_label: dec_label.c
	${CC} ${CFLAGS} -o dec_label dec_label.c

# utilities

clean cleandir:
	rm -f .depend *.o *.exe *.i errs make.out core*
	rm -f a.out ${ALL}
	rm -f boot[a-z][a-z] [a-z][a-z]boot
	rm -f libdrive.a
	cd libsa; make cleandir

install:
	./mkboot boot rzboot bootrz
	install -o bin -g bin -m 444 rzboot ${DESTDIR}/usr/mdec
	install -o bin -g bin -m 444 bootrz ${DESTDIR}/usr/mdec

depend: ${SRCS}
	mkdep ${INCPATH} ${DEFS} ${SRCS}
	mkdep -a -p ${INCPATH} ${DEFS} mkboot.c mkboottape.c
	cd libsa; make depend
