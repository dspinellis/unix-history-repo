#
# Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.c%
#
#	@(#)Makefile	7.2 (Berkeley) %G%
#
LIBDIR=/lib
DESTDIR=

#CC=occ -Dconst= -Dvolatile= -Dsigned= -Dinline=
#AS=oas

CC=cc -Dconst=__const__ -Dvolatile=__volatile__ \
	-Dsigned=__signed__ -Dinline=__inline__ -fpcc-struct-return
AS=as

INCPATH=-I. -I../sys -I..
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
	ld -N -T ${RELOC} -e begin srt0.o tboot.o -L${LIBDIR} ${LIBSA} -lc -o tboot
	@size tboot
	@echo tboot total size should not exceed 520192 bytes

tboot.o: boot.c ../h/param.h ../ufs/inode.h ../ufs/fs.h
tboot.o: saio.h ../h/reboot.h ../h/vm.h
	cp boot.c tboot.c; chmod +w tboot.c
	${CC} -c -O ${INCPATH} ${COPTS} -DJUSTASK tboot.c
	rm -f tboot.c

tcopy:	copy.o tpsrt0.o ${LIBSA}
	ld -N -T ${RELOC} -e begin tpsrt0.o copy.o -L${LIBDIR} ${LIBSA} -lc -o tcopy
	@size tcopy
	@echo tcopy total size should not exceed 520192 bytes

# bootable from floppy or real disks

dboot:	boot.o srt0.o bootconf.o ${LIBSA}
	ld -N -T ${RELOC} -e begin -o dboot srt0.o boot.o bootconf.o -L${LIBDIR} ${LIBSA} -lc
	@size dboot
	@echo dboot text+data size should not exceed 57344 bytes
	@echo dboot total size should not exceed 520192 bytes

bootconf.o: conf.o
	cp conf.c bootconf.c
	${CC} -c ${INCPATH} ${COPTS} -DBOOT bootconf.c
	rm -f bootconf.c

# non-secure boot

nboot:	nboot.o srt0.o bootconf.o ${LIBSA}
	ld -N -T ${RELOC} -e begin -o nboot srt0.o nboot.o bootconf.o -L${LIBDIR} ${LIBSA} -lc
	@size nboot
	@echo nboot text+data size should not exceed 57344 bytes
	@echo nboot total size should not exceed 520192 bytes

nboot.o: boot.c ../h/param.h ../ufs/inode.h ../ufs/fs.h
nboot.o: saio.h ../h/reboot.h ../h/vm.h
	cp boot.c nboot.c; chmod +w nboot.c
	${CC} -c -O ${INCPATH} ${COPTS} -DINSECURE ${DEFS} nboot.c
	rm -f nboot.c

dcopy:   copy.o srt0.o conf.o ${LIBSA}
	ld -N -T ${RELOC} -e begin -o dcopy srt0.o copy.o conf.o -L${LIBDIR} ${LIBSA} -lc

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

depend:
	for i in ${SRCS} ${DUMMIES}; do \
	    ${CC} -M ${INCPATH} ${DEFS} $$i | \
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
# DEPENDENCIES MUST END AT END OF FILE

sys.o : sys.c saio.h ../h/types.h ../h/param.h ../sys/syslimits.h \
  ../sys/types.h ../sys/signal.h /usr/include/machine/trap.h \
  /usr/include/machine/machparam.h /usr/include/machine/endian.h \
  /usr/include/machine/machlimits.h ../ufs/inode.h ../ufs/dinode.h \
  ../ufs/fs.h ../ufs/dir.h ../sys/stat.h 

conf.o : conf.c saio.h ../h/types.h ../h/param.h ../sys/syslimits.h \
  ../sys/types.h ../sys/signal.h /usr/include/machine/trap.h \
  /usr/include/machine/machparam.h /usr/include/machine/endian.h \
  /usr/include/machine/machlimits.h ../ufs/inode.h ../ufs/dinode.h \
  ../ufs/fs.h 

prf.o : prf.c ../h/param.h ../sys/syslimits.h ../sys/types.h \
  ../sys/signal.h /usr/include/machine/trap.h \
  /usr/include/machine/machparam.h /usr/include/machine/endian.h \
  /usr/include/machine/machlimits.h 

machdep.o : machdep.c ../sys/param.h ../sys/syslimits.h ../sys/types.h \
  ../sys/signal.h /usr/include/machine/trap.h \
  /usr/include/machine/machparam.h /usr/include/machine/endian.h \
  /usr/include/machine/machlimits.h 

autoconf.o : autoconf.c samachdep.h ../sys/param.h ../sys/syslimits.h \
  ../sys/types.h ../sys/signal.h /usr/include/machine/trap.h \
  /usr/include/machine/machparam.h /usr/include/machine/endian.h \
  /usr/include/machine/machlimits.h ../hpdev/device.h ../hpdev/grfvar.h 

hpib.o : hpib.c ../h/reboot.h ../hpdev/device.h hpibvar.h saio.h \
  ../h/types.h ../h/param.h ../sys/syslimits.h ../sys/types.h ../sys/signal.h \
  /usr/include/machine/trap.h /usr/include/machine/machparam.h \
  /usr/include/machine/endian.h /usr/include/machine/machlimits.h \
  ../ufs/inode.h ../ufs/dinode.h ../ufs/fs.h samachdep.h 

nhpib.o : nhpib.c ../sys/param.h ../sys/syslimits.h ../sys/types.h \
  ../sys/signal.h /usr/include/machine/trap.h \
  /usr/include/machine/machparam.h /usr/include/machine/endian.h \
  /usr/include/machine/machlimits.h ../hpdev/nhpibreg.h hpibvar.h 

fhpib.o : fhpib.c ../sys/param.h ../sys/syslimits.h ../sys/types.h \
  ../sys/signal.h /usr/include/machine/trap.h \
  /usr/include/machine/machparam.h /usr/include/machine/endian.h \
  /usr/include/machine/machlimits.h ../hpdev/fhpibreg.h hpibvar.h 

rd.o : rd.c saio.h ../h/types.h ../h/param.h ../sys/syslimits.h \
  ../sys/types.h ../sys/signal.h /usr/include/machine/trap.h \
  /usr/include/machine/machparam.h /usr/include/machine/endian.h \
  /usr/include/machine/machlimits.h ../ufs/inode.h ../ufs/dinode.h \
  ../ufs/fs.h samachdep.h ../hpdev/rdreg.h 

ct.o : ct.c ../h/types.h ../hpdev/ctreg.h saio.h ../h/param.h \
  ../sys/syslimits.h ../sys/types.h ../sys/signal.h \
  /usr/include/machine/trap.h /usr/include/machine/machparam.h \
  /usr/include/machine/endian.h /usr/include/machine/machlimits.h \
  ../ufs/inode.h ../ufs/dinode.h ../ufs/fs.h samachdep.h 

scsi.o : scsi.c ../h/types.h ../h/reboot.h ../hpdev/device.h \
  ../hpdev/scsireg.h ../sys/ioctl.h scsivar.h saio.h ../h/param.h \
  ../sys/syslimits.h ../sys/types.h ../sys/signal.h \
  /usr/include/machine/trap.h /usr/include/machine/machparam.h \
  /usr/include/machine/endian.h /usr/include/machine/machlimits.h \
  ../ufs/inode.h ../ufs/dinode.h ../ufs/fs.h samachdep.h 

sd.o : sd.c saio.h ../h/types.h ../h/param.h ../sys/syslimits.h \
  ../sys/types.h ../sys/signal.h /usr/include/machine/trap.h \
  /usr/include/machine/machparam.h /usr/include/machine/endian.h \
  /usr/include/machine/machlimits.h ../ufs/inode.h ../ufs/dinode.h \
  ../ufs/fs.h samachdep.h ../hpdev/scsireg.h ../sys/ioctl.h 

hil.o : hil.c samachdep.h 

cons.o : cons.c ../sys/param.h ../sys/syslimits.h ../sys/types.h \
  ../sys/signal.h /usr/include/machine/trap.h \
  /usr/include/machine/machparam.h /usr/include/machine/endian.h \
  /usr/include/machine/machlimits.h samachdep.h /usr/include/machine/cons.h 

ite.o : ite.c samachdep.h 

ite_subr.o : ite_subr.c samachdep.h 

ite_dv.o : ite_dv.c samachdep.h 

ite_gb.o : ite_gb.c samachdep.h 

ite_rb.o : ite_rb.c samachdep.h 

ite_tc.o : ite_tc.c samachdep.h 

dca.o : dca.c 
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
