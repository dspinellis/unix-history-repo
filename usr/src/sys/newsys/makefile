#
#	%M%		%I%	%G%
#
# Makefile for Device Drivers
#
IDENT=	-DUCB

COPTS=	${IDENT} -DVAX -DFASTVAX -DKERNEL -DCOCANET -DTMDUMP
CFLAGS=	-O ${COPTS}

FILES=	bio.o bk.o cons.o dh.o dhfdm.o dkleave.o dsort.o dz.o flp.o \
	hp.o ht.o mba.o mem.o mx1.o mx2.o sw.o tdump.o \
	tty.o ttynew.o uba.o up.o va.o vp.o dv.o tm.o lp.o

#
# All drivers need special optimizer processing and must be noted here
#
DRIVERS=dh.o dhdm.o dhfdm.o dz.o hk.o hp.o ht.o kl.o mba.o \
	rk.o rk7.o tdump.o uba.o up.o va.o vp.o dv.o tm.o lp.o

CFILES=	bio.c bk.c cons.c dh.c dhfdm.c dkleave.c dksort.c dz.c flp.c \
	hp.c ht.c mba.c mem.c mx1.c mx2.c sw.c tdump.c \
	tty.c ttynew.c uba.c up.c va.c vp.c dv.c tm.c lp.c

all: ${FILES}

${DRIVERS}:
	cc -c -S ${COPTS} $*.c
	/lib/c2 -i $*.s | sed -f ../coca/asm.sed | as -o $*.o
	rm $*.s

.c.o:
	cc -c -S ${COPTS} $*.c
	/lib/c2 $*.s | sed -f ../coca/asm.sed | as -o $*.o
	rm $*.s

clean:
	rm -f *.o

depend:
	grep '^#include' ${CFILES} \
		| sed 's/:[^"]*"\([^"]*\)".*/: \1/' \
		| sed 's/\.c/.o/' >makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp makefile makefile.bak
	ed - makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> makefile
	echo '# see make depend above' >> makefile

../ch/param.h: /usr/include/signal.h
../ch/tty.h: /usr/include/sgtty.h
../ch/tty.h: ../ch/ioctl.h
../ch/user.h: ../ch/pcb.h ../ch/dmap.h ../ch/vtimes.h /usr/include/errno.h

# DO NOT DELETE THIS LINE -- make depend uses it

bio.o: ../ch/param.h
bio.o: ../ch/systm.h
bio.o: ../ch/dir.h
bio.o: ../ch/user.h
bio.o: ../ch/buf.h
bio.o: ../ch/conf.h
bio.o: ../ch/proc.h
bio.o: ../ch/seg.h
bio.o: ../ch/pte.h
bio.o: ../ch/vm.h
bk.o: ../ch/param.h
bk.o: ../ch/systm.h
bk.o: ../ch/dir.h
bk.o: ../ch/user.h
bk.o: ../ch/tty.h
bk.o: ../ch/proc.h
bk.o: ../ch/mx.h
bk.o: ../ch/inode.h
bk.o: ../ch/file.h
bk.o: ../ch/conf.h
bk.o: ../ch/buf.h
cons.o: ../ch/param.h
cons.o: ../ch/conf.h
cons.o: ../ch/dir.h
cons.o: ../ch/user.h
cons.o: ../ch/tty.h
cons.o: ../ch/systm.h
cons.o: ../ch¯½1ns.h
cons.o: ../ch/mtpr.h
dh.o: ../ch/param.h
dh.o: ../ch/conf.h
dh.o: ../ch/dir.h
dh.o: ../ch/user.h
dh.o: ../ch/tty.h
dh.o: ../ch/map.h
dh.o: ../ch/pte.h
dh.o: ../ch/uba.h
dh.o: ../ch/bk.h
dhfdm.o: ../ch/param.h
dhfdm.o: ../ch/tty.h
dhfdm.o: ../ch/conf.h
dkleave.o: ../ch/param.h
dkleave.o: ../ch/buf.h
dz.o: ../ch/param.h
dz.o: ../ch/systm.h
dz.o: ../ch/tty.h
dz.o: ../ch/dir.h
dz.o: ../ch/user.h
dz.o: ../ch/map.h
dz.o: ../ch/pte.h
dz.o: ../ch/uba.h
dz.o: ../ch/conf.h
dz.o: ../ch/pdma.h
dz.o: ../ch/bk.h
flp.o: ../ch/flp.h
flp.o: ../ch/param.h
flp.o: ../ch/systm.h
flp.o: ../ch/conf.h
flp.o: ../ch/dir.h
flp.o: ../ch/user.h
flp.o: ../ch/mtpr.h
flp.o: ../ch/buf.h
flp.o: ../ch/cons.h
hp.o: ../ch/param.h
hp.o: ../ch/systm.h
hp.o: ../ch/dk.h
hp.o: ../ch/buf.h
hp.o: ../ch/conf.h
hp.o: ../ch/dir.h
hp.o: ../ch/user.h
hp.o: ../ch/map.h
hp.o: ../ch/pte.h
hp.o: ../ch/mba.h
hp.o: ../ch/mtpr.h
hp.o: ../ch/vm.h
ht.o: ../ch/param.h
ht.o: ../ch/systm.h
ht.o: ../ch/buf.h
ht.o: ../ch/conf.h
ht.o: ../ch/dir.h
ht.o: ../ch/file.h
ht.o: ../ch/user.h
ht.o: ../ch/map.h
ht.o: ../ch/pte.h
ht.o: ../ch/mba.h
mba.o: ../ch/param.h
mba.o: ../ch/buf.h
mba.o: ../ch/conf.h
mba.o: ../ch/systm.h
mba.o: ../ch/dir.h
mba.o: ../ch/user.h
mba.o: ../ch/proc.h
mba.o: ../ch/seg.h
mba.o: ../ch/pte.h
mba.o: ../ch/map.h
mba.o: ../ch/mba.h
mba.o: ../ch/mtpr.h
mba.o: ../ch/vm.h
mem.o: ../ch/param.h
mem.o: ../ch/dir.h
mem.o: ../ch/user.h
mem.o: ../ch/conf.h
mem.o: ../ch/buf.h
mem.o: ../ch/systm.h
mem.o: ../ch/pte.h
mem.o: ../ch/mtpr.h
mem.o: ../ch/vm.h
mem.o: ../ch/cmap.h
mx1.o: ../ch/param.h
mx1.o: ../ch/systm.h
mx1.o: ../ch/dir.h
mx1.o: ../ch/user.h
mx1.o: ../ch/reg.h
mx1.o: ../ch/proc.h
mx1.o: ../ch/tty.h
mx1.o: ../ch/inode.h
mx1.o: ../ch/mx.h
mx1.o: ../ch/file.h
mx1.o: ../ch/conf.h
mx2.o: ../ch/param.h
mx2.o: ../ch/systm.h
mx2.o: ../ch/dir.h
mx2.o: ../ch/user.h
mx2.o: ../ch/proc.h
mx2.o: ../ch/tty.h
mx2.o: ../ch/inode.h
mx2.o: ../ch/mx.h
mx2.o: ../ch/file.h
mx2.o: ../ch/conf.h
mx2.o: ../ch/buf.h
sw.o: ../ch/param.h
sw.o: ../ch/systm.h
sw.o: ../ch/buf.h
sw.o: ../ch/conf.h
sw.o: ../ch/dir.h
sw.o: ../ch/user.h
sw.o: ../ch/inode.h
sw.o: ../ch/map.h
tdump.o: ../ch/param.h
tdump.o: ../ch/vm.h
tdump.o: ../ch/pte.h
tdump.o: ../ch/map.h
tdump.o: ../ch/uba.h
tdump.o: ../ch/systm.h
tdump.o: ../ch/cmap.h
tdump.o: ../ch/mba.h
tty.o: ../ch/param.h
tty.o: ../ch/systm.h
tty.o: ../ch/dir.h
tty.o: ../ch/user.h
tty.o: ../ch/tty.h
tty.o: ../ch/proc.h
tty.o: ../ch/mx.h
tty.o: ../ch/inode.h
tty.o: ../ch/file.h
tty.o: ../ch/reg.h
tty.o: ../ch/conf.h
tty.o: ../ch/buf.h
tty.o: ../ch/dk.h
ttynew.o: ../ch/param.h
ttynew.o: ../ch/systm.h
ttynew.o: ../ch/dir.h
ttynew.o: ../ch/user.h
ttynew.o: ../ch/tty.h
ttynew.o: ../ch/proc.h
ttynew.o: ../ch/mx.h
ttynew.o: ../ch/inode.h
ttynew.o: ../ch/file.h
ttynew.o: ../ch/reg.h
ttynew.o: ../ch/conf.h
ttynew.o: ../ch/buf.h
ttynew.o: ../ch/dk.h
uba.o: ../ch/param.h
uba.o: ../ch/map.h
uba.o: ../ch/pte.h
uba.o: ../ch/uba.h
uba.o: ../ch/buf.h
uba.o: ../ch/dir.h
uba.o: ../ch/user.h
uba.o: ../ch/proc.h
uba.o: ../ch/vm.h
uba.o: ../ch/conf.h
up.o: ../ch/param.h
up.o: ../ch/systm.h
up.o: ../ch/dk.h
up.o: ../ch/buf.h
up.o: ../ch/conf.h
up.o: ../ch/dir.h
up.o: ../ch/user.h
up.o: ../ch/map.h
up.o: ../ch/mba.h
up.o: ../ch/mtpr.h
up.o: ../ch/pte.h
up.o: ../ch/uba.h
up.o: ../ch/vm.h
va.o: ../ch/param.h
va.o: ../ch/dir.h
va.o: ../ch/user.h
va.o: ../ch/buf.h
va.o: ../ch/systm.h
va.o: ../ch/map.h
va.o: ../ch/pte.h
va.o: ../ch/uba.h
va.o: ../ch/vcmd.h
vp.o: ../ch/param.h
vp.o: ../ch/dir.h
vp.o: ../ch/user.h
vp.o: ../ch/buf.h
vp.o: ../ch/systm.h
vp.o: ../ch/map.h
vp.o: ../ch/pte.h
vp.o: ../ch/uba.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
