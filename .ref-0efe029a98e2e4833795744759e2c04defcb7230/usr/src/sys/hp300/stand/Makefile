#
# Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)Makefile	7.4 (Berkeley) %G%
#
DESTDIR=

#CC=occ -Dconst= -Dvolatile= -Dsigned= -Dinline=
#AS=oas

CC=cc -Dconst=__const__ -Dvolatile=__volatile__ \
	-Dsigned=__signed__ -Dinline=__inline__ -fpcc-struct-return
AS=as

INCPATH=-I. -I/usr/include -I../sys -I..
CONS= -DDCACONSOLE -DITECONSOLE -DDCMCONSOLE
DEFS= -DSTANDALONE ${CONS} # -DROMPRF # -DDEBUG
COPTS=
CFLAGS=	-O ${INCPATH} ${DEFS} ${COPTS}
# RELOC=FFF80000 allows for boot prog up to 7F000 (520192) bytes long
RELOC=	FFF80000

SRCS=	sys.c conf.c prf.c machdep.c autoconf.c \
	hpib.c nhpib.c fhpib.c rd.c ct.c scsi.c sd.c hil.c \
	cons.c ite.c ite_subr.c ite_dv.c ite_gb.c ite_rb.c ite_tc.c dca.c dcm.c
DUMMIES=
DRIVERS=autoconf.o hpib.o nhpib.o fhpib.o rd.o ct.o scsi.o sd.o hil.o \
	cons.o ite.o ite_subr.o ite_dv.o ite_gb.o ite_rb.o ite_tc.o dca.o dcm.o
LIBSA=	libsa.a

ALL=	dboot nboot tboot tcopy

all: ${ALL}

${LIBSA}: sys.o conf.o ${DRIVERS} prf.o machdep.o
	ar crv ${LIBSA} $?
	ranlib ${LIBSA}

${DRIVERS}: samachdep.h
	${CC} -c ${CFLAGS} $*.c

# startups

srt0.o: srt0.c
	${CC} -E ${INCPATH} ${DEFS} ${COPTS} srt0.c | ${AS} -o srt0.o

tpsrt0.o: srt0.c
	${CC} -E ${INCPATH} ${DEFS} ${COPTS} -DTP srt0.c | ${AS} -o tpsrt0.o

# bootable from tape

tboot:	tboot.o srt0.o ${LIBSA}
	ld -N -T ${RELOC} -e begin srt0.o tboot.o ${LIBSA} -lc -o tboot
	@size tboot
	@echo tboot total size should not exceed 520192 bytes

tboot.o: boot.o
	cp boot.c tboot.c; chmod +w tboot.c
	${CC} -c -O ${INCPATH} ${COPTS} -DJUSTASK tboot.c
	rm -f tboot.c

tcopy:	copy.o tpsrt0.o ${LIBSA}
	ld -N -T ${RELOC} -e begin tpsrt0.o copy.o ${LIBSA} -lc -o tcopy
	@size tcopy
	@echo tcopy total size should not exceed 520192 bytes

# bootable from floppy or real disks

dboot:	boot.o srt0.o bootconf.o ${LIBSA}
	ld -N -T ${RELOC} -e begin -o dboot srt0.o boot.o bootconf.o ${LIBSA} -lc
	@size dboot
	@echo dboot text+data size should not exceed 57344 bytes
	@echo dboot total size should not exceed 520192 bytes

bootconf.o: conf.o
	cp conf.c bootconf.c
	${CC} -c ${INCPATH} ${COPTS} -DBOOT bootconf.c
	rm -f bootconf.c

# non-secure boot

nboot:	nboot.o srt0.o bootconf.o ${LIBSA}
	ld -N -T ${RELOC} -e begin -o nboot srt0.o nboot.o bootconf.o ${LIBSA} -lc
	@size nboot
	@echo nboot text+data size should not exceed 57344 bytes
	@echo nboot total size should not exceed 520192 bytes

nboot.o: boot.o
	cp boot.c nboot.c; chmod +w nboot.c
	${CC} -c -O ${INCPATH} ${COPTS} -DINSECURE ${DEFS} nboot.c
	rm -f nboot.c

dcopy:   copy.o srt0.o conf.o ${LIBSA}
	ld -N -T ${RELOC} -e begin -o dcopy srt0.o copy.o conf.o ${LIBSA} -lc

mkboot: mkboot.c
	${CC} ${CFLAGS} -o mkboot mkboot.c

installboot: installboot.c
	${CC} ${CFLAGS} -o installboot installboot.c

# utilities

print:
	@pr makefile
	@ls -l | pr 
	@pr *.h *.c

clean:
	rm -f *.o *.exe *.i errs make.out
	rm -f a.out boot cat tpboot tpcopy copy tpformat
	rm -f boot[a-z]? boot[a-wyz][a-z].c conf[a-wyz][a-z].c
	rm -f format drtest core sboot bootconf.c
	rm -f libsa.a mkboot tboot tcopy dboot nboot installboot *.lif

lint:
	lint ${INCPATH} ${COPTS} -hxbn boot.c ${SRCS} | \
	    grep -v 'possible pointer alignment' | \
	    grep -v 'struct/union .* never defined'

install: mkboot installboot ${ALL}
	mkboot dboot diskboot.lif
	mkboot nboot ndiskboot.lif
	mkboot tboot tcopy tapeboot.lif
	install -c -s installboot ${DESTDIR}/usr/mdec
	install -c -m 644 diskboot.lif ${DESTDIR}/usr/mdec/bootrd
	rm -f ${DESTDIR}/usr/mdec/bootsd
	ln ${DESTDIR}/usr/mdec/bootrd ${DESTDIR}/usr/mdec/bootsd
	install -c -m 644 tapeboot.lif ${DESTDIR}/usr/mdec/bootct
	mv diskboot.lif ndiskboot.lif tapeboot.lif ${DESTDIR}/sys/hpdist/tp

depend: ${SRCS}
	mkdep ${INCPATH} -DSTANDALONE ${SRCS} ${DUMMIES}
