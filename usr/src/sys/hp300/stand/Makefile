#	@(#)Makefile	7.16 (Berkeley) %G%

DESTDIR=
INCPATH=-I../..
VPATH=	../../stand

CC=cc
AS=as

# RELOC=FFF80000 allows for boot prog up to 7F000 (520192) bytes long
RELOC=	FFF80000

CONS= -DDCACONSOLE -DITECONSOLE -DDCMCONSOLE
DEFS= -DSTANDALONE ${CONS} # -DROMPRF
CFLAGS=	-O ${INCPATH} ${DEFS}

DRIVERS=autoconf.c cons.c ct.c dca.c dcm.c fhpib.c hil.c hpib.c ite.c \
	ite_dv.c ite_gb.c ite_hy.c ite_rb.c ite_subr.c ite_tc.c nhpib.c rd.c \
	scsi.c sd.c
SRCS=	conf.c machdep.c prf.c ${DRIVERS}
LIBS=	libsa/libsa.a libdrive.a ${DESTDIR}/usr/lib/libgnulib.a \
	${DESTDIR}/usr/lib/libc.a

ALL=	dboot nboot tboot tcopy mkboot installboot

all: ${ALL}

dboot nboot tboot tcopy: ${LIBS}

libsa/libsa.a::
	cd libsa; make

libdrive.a: conf.o machdep.o prf.o ${DRIVERS:.c=.o}
	ar crv $@ $?
	ranlib $@

${DRIVERS}: samachdep.h

# depend on DEFS

machdep.o srt0.o: Makefile
cons.o dca.o hil.o: Makefile
ite.o ite_subr.o ite_dv.o ite_gb.o ite_hy.o ite_rb.o ite_tc.o: Makefile

# startups

srt0.o: srt0.c
	${CC} -E ${INCPATH} ${DEFS} srt0.c | ${AS} -o srt0.o

tpsrt0.o: srt0.c
	${CC} -E ${INCPATH} ${DEFS} -DTP srt0.c | ${AS} -o tpsrt0.o

# bootable from tape

tboot:	tboot.o srt0.o ${LIBS}
	ld -N -T ${RELOC} -e begin srt0.o tboot.o ${LIBS} -o $@
	@size tboot
	@echo tboot total size should not exceed 520192 bytes

tboot.o: boot.o
	cp boot.c tboot.c; chmod +w tboot.c
	${CC} -c ${CFLAGS} -DJUSTASK tboot.c
	rm -f tboot.c

tcopy:	copy.o tpsrt0.o ${LIBS}
	ld -N -T ${RELOC} -e begin tpsrt0.o copy.o ${LIBS} -o $@
	@size tcopy
	@echo tcopy total size should not exceed 520192 bytes

# bootable from floppy or real disks

dboot:	boot.o srt0.o bootconf.o ${LIBS}
	ld -N -T ${RELOC} -e begin srt0.o boot.o bootconf.o ${LIBS} -o $@ 
	@size dboot
	@echo dboot text+data size should not exceed 57344 bytes
	@echo dboot total size should not exceed 520192 bytes

bootconf.o: conf.o
	rm -f bootconf.c
	ln -s conf.c bootconf.c
	${CC} -c ${CFLAGS} -DBOOT bootconf.c
	rm -f bootconf.c

# non-secure boot (allows booting non-root owned or world writable kernels)

nboot:	nboot.o srt0.o bootconf.o ${LIBS}
	ld -N -T ${RELOC} -e begin -o nboot srt0.o nboot.o bootconf.o ${LIBS}
	@size nboot
	@echo nboot text+data size should not exceed 57344 bytes
	@echo nboot total size should not exceed 520192 bytes

nboot.o: boot.o
	rm -f nboot.c
	ln -s boot.c nboot.c
	${CC} -c ${CFLAGS} -DINSECURE nboot.c
	rm -f nboot.c

dcopy:	copy.o srt0.o conf.o ${LIBS}
	ld -N -T ${RELOC} -e begin -o dcopy srt0.o copy.o conf.o ${LIBS}

mkboot: mkboot.c
	${CC} ${CFLAGS} -o mkboot mkboot.c

installboot: installboot.c
	${CC} ${CFLAGS} -o installboot installboot.c

# utilities

clean cleandir:
	rm -f .depend *.o *.exe *.i errs make.out
	rm -f a.out boot cat tpboot tpcopy copy tpformat
	rm -f boot[a-z]? boot[a-wyz][a-z].c conf[a-wyz][a-z].c
	rm -f format drtest core sboot bootconf.c nboot.c
	rm -f libdrive.a mkboot tboot tcopy dboot nboot installboot *.lif
	cd libsa; make cleandir

install:
	./mkboot dboot diskboot.lif
	./mkboot nboot ndiskboot.lif
	./mkboot tboot tcopy tapeboot.lif
	install -s -o bin -g bin -m 555 installboot ${DESTDIR}/usr/mdec
	install -o bin -g bin -m 444 diskboot.lif ${DESTDIR}/usr/mdec/bootrd
	rm -f ${DESTDIR}/usr/mdec/bootsd
	ln ${DESTDIR}/usr/mdec/bootrd ${DESTDIR}/usr/mdec/bootsd
	install -o bin -g bin -m 444 tapeboot.lif ${DESTDIR}/usr/mdec/bootct
#	mv diskboot.lif ndiskboot.lif tapeboot.lif ${DESTDIR}/sys/hpdist/tp

depend: ${SRCS}
	mkdep ${INCPATH} ${DEFS} ${SRCS}
	mkdep -a -p ${INCPATH} ${DEFS} mkboot.c installboot.c
	cd libsa; make depend
