#
#	%M%	%I%	%H%
#
# Makefile for Berkeley Virtual Vax/Unix, September, 1979
#
# Modified from Holmdel's UNIX 32/V:
#	Virtual UNIX Mods by Ozalp Babaoglu, Bill Joy and Juan Porcar
#	Unibus Adapter Mods (driving unibus disks) by Eric Allman
#	Console Floppy Driver by Richard Tuck and Keith Sklower
#
IDENT=	#-DUCB -DERNIE

COPTS=	${IDENT} -DKERNEL
CFLAGS=	-O ${COPTS}
AHEADS=	../h/mtpr.m ../h/trap.m ../h/psl.m ../h/pcb.m ../h/clock.m \
	../h/cons.m ../h/page.m ../h/mba.m ../h/uba.m ../h/cmap.m

FILES=	locore.o acct.o alloc.o bio.o clock.o conf.o cons.o \
	dkleave.o dsort.o dz.o fio.o flp.o hp.o ht.o iget.o kl.o \
	machdep.o main.o malloc.o mba.o mem.o mx1.o mx2.o nami.o \
	partab.o pipe.o prf.o prim.o rdwri.o rp.o sig.o slp.o \
	subr.o sys.o sys1.o sys2.o sys3.o sys4.o sysent.o tdump.o \
	text.o trap.o tty.o uba.o univec.o vmdrum.o vmmem.o vmmon.o vmpage.o \
	vmproc.o vmpt.o vmsched.o vmsubr.o vmswap.o vmsys.o vmsysent.o vp.o

#
# Drivers need special optimizer processing and must be noted here
#
DRIVERS=dz.o dzk.o hp.o ht.o kl.o mba.o rm.o rp.o tdump.o uba.o vp.o

CFILES= acct.c alloc.c bio.c clock.c conf.c cons.c \
	dkleave.c dsort.c dz.c fio.c flp.c hp.c ht.c iget.c kl.c \
	machdep.c main.c malloc.c mba.c mem.c mx1.c mx2.c nami.c \
	partab.c pipe.c prf.c prim.c rdwri.c rp.c sig.c slp.c \
	subr.c sys.c sys1.c sys2.c sys3.c sys4.c sysent.c tdump.c \
	text.c trap.c tty.c uba.c univec.c vmdrum.c vmmem.c vmmon.c vmpage.c \
	vmproc.c vmpt.c vmsched.c vmsubr.c vmswap.c vmsys.c vmsysent.c vp.c

vmunix: ${FILES} symbols.sort
	@echo loading...
	@ld -o vmunix -e start -x -T 80000000 ${FILES}
	@echo rearranging symbols...
	@-/usr/ucb/symorder symbols.sort vmunix
	size vmunix
	@chmod 755 vmunix
	ls -l vmunix

warning:
	echo Make sure you have done a 'make depend' before
	echo 'make all' if you have changed any dependencies.

clean:
	rm -f eddep vmunix tags *.o

lint:
	@lint -hbxn ${COPTS} Locore.c ${CFILES} |\
		grep -v 'struct/union .* never defined' |\
		grep -v 'possible pointer alignment problem'

${DRIVERS}:
	cc -c -S ${COPTS} $*.c
	/lib/c2 -i $*.s $*.os
	as -o $*.o $*.os
	rm $*.s $*.os

locore.o: ${AHEADS}
	@echo as -o locore.o AHEADS locore.s
	@as -o locore.o ${AHEADS} locore.s

depend:
	grep '^#include' ${CFILES} | sed 's/:[^"]*"\([^"]*\)".*/: \1/' \
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

symbols.sort: symbols.raw
	grep -v '^#' symbols.raw | sed 's/^	//' | sort | uniq >symbols.sort

tags:
	/usr/ucb/ctags ${CFILES}

print:
	@pr makefile TODO linterrs
	@/usr/ucb/cxref ${CFILES} | pr -h XREF
	@(size -l vmunix ; size *.o) | pr -h sizes
	@ls -ls | pr
	@cd ../h; ls -ls | pr ; pr [a-l]*.h m[a-wyz]*.h [n-z]*.h ?[a-z]*.m
	@pr Locore.c ?[a-z]*.s ${CFILES}

../h/user.h: ../h/pcb.h ../h/dmap.h
sysent.c: vmsysent.c

# DO NOT DELETE THIS LINE -- make depend uses it

acct.o: ../h/param.h
acct.o: ../h/systm.h
acct.o: ../h/acct.h
acct.o: ../h/dir.h
acct.o: ../h/user.h
acct.o: ../h/inode.h
acct.o: ../h/proc.h
acct.o: ../h/seg.h
alloc.o: ../h/param.h
alloc.o: ../h/systm.h
alloc.o: ../h/mount.h
alloc.o: ../h/filsys.h
alloc.o: ../h/fblk.h
alloc.o: ../h/conf.h
alloc.o: ../h/buf.h
alloc.o: ../h/inode.h
alloc.o: ../h/ino.h
alloc.o: ../h/dir.h
alloc.o: ../h/user.h
bio.o: ../h/param.h
bio.o: ../h/systm.h
bio.o: ../h/dir.h
bio.o: ../h/user.h
bio.o: ../h/buf.h
bio.o: ../h/conf.h
bio.o: ../h/proc.h
bio.o: ../h/seg.h
bio.o: ../h/pte.h
bio.o: ../h/vm.h
clock.o: ../h/param.h
clock.o: ../h/systm.h
clock.o: ../h/callo.h
clock.o: ../h/seg.h
clock.o: ../h/dir.h
clock.o: ../h/user.h
clock.o: ../h/proc.h
clock.o: ../h/reg.h
clock.o: ../h/psl.h
clock.o: ../h/vm.h
clock.o: ../h/buf.h
conf.o: ../h/param.h
conf.o: ../h/systm.h
conf.o: ../h/buf.h
conf.o: ../h/tty.h
conf.o: ../h/conf.h
conf.o: ../h/proc.h
conf.o: ../h/text.h
conf.o: ../h/dir.h
conf.o: ../h/user.h
conf.o: ../h/file.h
conf.o: ../h/inode.h
conf.o: ../h/acct.h
conf.o: ../h/mba.h
cons.o: ../h/param.h
cons.o: ../h/conf.h
cons.o: ../h/dir.h
cons.o: ../h/user.h
cons.o: ../h/tty.h
cons.o: ../h/systm.h
cons.o: ../h/cons.h
cons.o: ../h/mtpr.h
dkleave.o: ../h/param.h
dkleave.o: ../h/buf.h
dsort.o: ../h/param.h
dsort.o: ../h/systm.h
dsort.o: ../h/buf.h
dz.o: ../h/param.h
dz.o: ../h/systm.h
dz.o: ../h/tty.h
dz.o: ../h/dir.h
dz.o: ../h/user.h
dz.o: ../h/map.h
dz.o: ../h/pte.h
dz.o: ../h/uba.h
dz.o: ../h/conf.h
dz.o: ../h/pdma.h
fio.o: ../h/param.h
fio.o: ../h/systm.h
fio.o: ../h/dir.h
fio.o: ../h/user.h
fio.o: ../h/filsys.h
fio.o: ../h/file.h
fio.o: ../h/conf.h
fio.o: ../h/inode.h
fio.o: ../h/reg.h
fio.o: ../h/acct.h
flp.o: ../h/flp.h
flp.o: ../h/param.h
flp.o: ../h/systm.h
flp.o: ../h/conf.h
flp.o: ../h/dir.h
flp.o: ../h/user.h
flp.o: ../h/mtpr.h
flp.o: ../h/buf.h
flp.o: ../h/cons.h
hp.o: ../h/param.h
hp.o: ../h/systm.h
hp.o: ../h/buf.h
hp.o: ../h/conf.h
hp.o: ../h/dir.h
hp.o: ../h/user.h
hp.o: ../h/map.h
hp.o: ../h/mba.h
hp.o: ../h/mtpr.h
hp.o: ../h/pte.h
ht.o: ../h/param.h
ht.o: ../h/systm.h
ht.o: ../h/buf.h
ht.o: ../h/conf.h
ht.o: ../h/dir.h
ht.o: ../h/file.h
ht.o: ../h/user.h
ht.o: ../h/map.h
ht.o: ../h/mba.h
iget.o: ../h/param.h
iget.o: ../h/systm.h
iget.o: ../h/mount.h
iget.o: ../h/dir.h
iget.o: ../h/user.h
iget.o: ../h/inode.h
iget.o: ../h/ino.h
iget.o: ../h/filsys.h
iget.o: ../h/conf.h
iget.o: ../h/buf.h
kl.o: ../h/param.h
kl.o: ../h/conf.h
kl.o: ../h/dir.h
kl.o: ../h/user.h
kl.o: ../h/tty.h
kl.o: ../h/systm.h
kl.o: ../h/map.h
kl.o: ../h/pte.h
kl.o: ../h/uba.h
machdep.o: ../h/param.h
machdep.o: ../h/systm.h
machdep.o: ../h/dir.h
machdep.o: ../h/user.h
machdep.o: ../h/map.h
machdep.o: ../h/reg.h
machdep.o: ../h/mtpr.h
machdep.o: ../h/clock.h
machdep.o: ../h/pte.h
machdep.o: ../h/vm.h
main.o: ../h/param.h
main.o: ../h/systm.h
main.o: ../h/dir.h
main.o: ../h/user.h
main.o: ../h/filsys.h
main.o: ../h/mount.h
main.o: ../h/map.h
main.o: ../h/proc.h
main.o: ../h/inode.h
main.o: ../h/seg.h
main.o: ../h/conf.h
main.o: ../h/buf.h
main.o: ../h/mtpr.h
main.o: ../h/pte.h
main.o: ../h/clock.h
main.o: ../h/vm.h
main.o: ../h/cmap.h
malloc.o: ../h/param.h
malloc.o: ../h/systm.h
malloc.o: ../h/map.h
malloc.o: ../h/proc.h
malloc.o: ../h/dir.h
malloc.o: ../h/user.h
malloc.o: ../h/mtpr.h
malloc.o: ../h/text.h
mba.o: ../h/param.h
mba.o: ../h/buf.h
mba.o: ../h/conf.h
mba.o: ../h/systm.h
mba.o: ../h/dir.h
mba.o: ../h/user.h
mba.o: ../h/proc.h
mba.o: ../h/seg.h
mba.o: ../h/pte.h
mba.o: ../h/map.h
mba.o: ../h/mba.h
mba.o: ../h/mtpr.h
mba.o: ../h/vm.h
mem.o: ../h/param.h
mem.o: ../h/dir.h
mem.o: ../h/user.h
mem.o: ../h/conf.h
mem.o: ../h/buf.h
mem.o: ../h/systm.h
mem.o: ../h/pte.h
mem.o: ../h/mtpr.h
mem.o: ../h/vm.h
mem.o: ../h/cmap.h
mx1.o: ../h/param.h
mx1.o: ../h/systm.h
mx1.o: ../h/dir.h
mx1.o: ../h/user.h
mx1.o: ../h/reg.h
mx1.o: ../h/proc.h
mx1.o: ../h/tty.h
mx1.o: ../h/inode.h
mx1.o: ../h/mx.h
mx1.o: ../h/file.h
mx1.o: ../h/conf.h
mx2.o: ../h/param.h
mx2.o: ../h/systm.h
mx2.o: ../h/dir.h
mx2.o: ../h/user.h
mx2.o: ../h/proc.h
mx2.o: ../h/tty.h
mx2.o: ../h/inode.h
mx2.o: ../h/mx.h
mx2.o: ../h/file.h
mx2.o: ../h/conf.h
mx2.o: ../h/buf.h
nami.o: ../h/param.h
nami.o: ../h/systm.h
nami.o: ../h/inode.h
nami.o: ../h/mount.h
nami.o: ../h/dir.h
nami.o: ../h/user.h
nami.o: ../h/buf.h
pipe.o: ../h/param.h
pipe.o: ../h/systm.h
pipe.o: ../h/dir.h
pipe.o: ../h/user.h
pipe.o: ../h/inode.h
pipe.o: ../h/file.h
pipe.o: ../h/reg.h
prf.o: ../h/param.h
prf.o: ../h/systm.h
prf.o: ../h/seg.h
prf.o: ../h/buf.h
prf.o: ../h/conf.h
prim.o: ../h/param.h
prim.o: ../h/tty.h
prim.o: ../h/systm.h
prim.o: ../h/conf.h
prim.o: ../h/buf.h
rdwri.o: ../h/param.h
rdwri.o: ../h/systm.h
rdwri.o: ../h/inode.h
rdwri.o: ../h/dir.h
rdwri.o: ../h/user.h
rdwri.o: ../h/buf.h
rdwri.o: ../h/conf.h
rp.o: ../h/mba.h
rp.o: ../h/param.h
rp.o: ../h/buf.h
rp.o: ../h/conf.h
rp.o: ../h/systm.h
rp.o: ../h/dir.h
rp.o: ../h/user.h
rp.o: ../h/map.h
rp.o: ../h/pte.h
rp.o: ../h/uba.h
sig.o: ../h/param.h
sig.o: ../h/systm.h
sig.o: ../h/dir.h
sig.o: ../h/user.h
sig.o: ../h/proc.h
sig.o: ../h/inode.h
sig.o: ../h/reg.h
sig.o: ../h/text.h
sig.o: ../h/seg.h
sig.o: ../h/mtpr.h
sig.o: ../h/pte.h
sig.o: ../h/psl.h
sig.o: ../h/vm.h
sig.o: ../h/buf.h
slp.o: ../h/param.h
slp.o: ../h/systm.h
slp.o: ../h/dir.h
slp.o: ../h/user.h
slp.o: ../h/proc.h
slp.o: ../h/file.h
slp.o: ../h/inode.h
slp.o: ../h/vm.h
slp.o: ../h/pte.h
subr.o: ../h/param.h
subr.o: ../h/systm.h
subr.o: ../h/conf.h
subr.o: ../h/inode.h
subr.o: ../h/dir.h
subr.o: ../h/user.h
subr.o: ../h/buf.h
sys.o: ../h/param.h
sys.o: ../h/systm.h
sys.o: ../h/conf.h
sys.o: ../h/dir.h
sys.o: ../h/user.h
sys.o: ../h/tty.h
sys.o: ../h/proc.h
sys1.o: ../h/param.h
sys1.o: ../h/systm.h
sys1.o: ../h/map.h
sys1.o: ../h/mtpr.h
sys1.o: ../h/dir.h
sys1.o: ../h/user.h
sys1.o: ../h/proc.h
sys1.o: ../h/buf.h
sys1.o: ../h/reg.h
sys1.o: ../h/inode.h
sys1.o: ../h/seg.h
sys1.o: ../h/acct.h
sys1.o: ../h/pte.h
sys1.o: ../h/vm.h
sys1.o: ../h/text.h
sys2.o: ../h/param.h
sys2.o: ../h/systm.h
sys2.o: ../h/dir.h
sys2.o: ../h/user.h
sys2.o: ../h/reg.h
sys2.o: ../h/file.h
sys2.o: ../h/inode.h
sys2.o: ../h/pte.h
sys2.o: ../h/vm.h
sys2.o: ../h/buf.h
sys2.o: ../h/mtpr.h
sys2.o: ../h/proc.h
sys3.o: ../h/param.h
sys3.o: ../h/systm.h
sys3.o: ../h/mount.h
sys3.o: ../h/ino.h
sys3.o: ../h/reg.h
sys3.o: ../h/buf.h
sys3.o: ../h/filsys.h
sys3.o: ../h/dir.h
sys3.o: ../h/user.h
sys3.o: ../h/inode.h
sys3.o: ../h/file.h
sys3.o: ../h/conf.h
sys3.o: ../h/stat.h
sys4.o: ../h/param.h
sys4.o: ../h/systm.h
sys4.o: ../h/dir.h
sys4.o: ../h/user.h
sys4.o: ../h/reg.h
sys4.o: ../h/inode.h
sys4.o: ../h/proc.h
sys4.o: ../h/clock.h
sys4.o: ../h/mtpr.h
sys4.o: ../h/timeb.h
sysent.o: ../h/param.h
sysent.o: ../h/systm.h
tdump.o: ../h/param.h
tdump.o: ../h/vm.h
tdump.o: ../h/pte.h
tdump.o: ../h/systm.h
tdump.o: ../h/cmap.h
text.o: ../h/param.h
text.o: ../h/systm.h
text.o: ../h/map.h
text.o: ../h/dir.h
text.o: ../h/user.h
text.o: ../h/proc.h
text.o: ../h/text.h
text.o: ../h/inode.h
text.o: ../h/buf.h
text.o: ../h/seg.h
text.o: ../h/pte.h
text.o: ../h/mtpr.h
text.o: ../h/vm.h
text.o: ../h/cmap.h
trap.o: ../h/param.h
trap.o: ../h/systm.h
trap.o: ../h/dir.h
trap.o: ../h/user.h
trap.o: ../h/proc.h
trap.o: ../h/reg.h
trap.o: ../h/seg.h
trap.o: ../h/trap.h
trap.o: ../h/psl.h
tty.o: ../h/param.h
tty.o: ../h/systm.h
tty.o: ../h/dir.h
tty.o: ../h/user.h
tty.o: ../h/tty.h
tty.o: ../h/proc.h
tty.o: ../h/mx.h
tty.o: ../h/inode.h
tty.o: ../h/file.h
tty.o: ../h/reg.h
tty.o: ../h/conf.h
tty.o: ../h/buf.h
uba.o: ../h/param.h
uba.o: ../h/map.h
uba.o: ../h/pte.h
uba.o: ../h/uba.h
uba.o: ../h/buf.h
uba.o: ../h/dir.h
uba.o: ../h/user.h
uba.o: ../h/proc.h
uba.o: ../h/vm.h
univec.o: ../h/param.h
univec.o: ../h/vm.h
vmdrum.o: ../h/param.h
vmdrum.o: ../h/systm.h
vmdrum.o: ../h/proc.h
vmdrum.o: ../h/dir.h
vmdrum.o: ../h/user.h
vmdrum.o: ../h/buf.h
vmdrum.o: ../h/text.h
vmdrum.o: ../h/map.h
vmdrum.o: ../h/pte.h
vmdrum.o: ../h/vm.h
vmdrum.o: ../h/cmap.h
vmmem.o: ../h/param.h
vmmem.o: ../h/systm.h
vmmem.o: ../h/pte.h
vmmem.o: ../h/cmap.h
vmmem.o: ../h/proc.h
vmmem.o: ../h/dir.h
vmmem.o: ../h/user.h
vmmem.o: ../h/mtpr.h
vmmem.o: ../h/text.h
vmmem.o: ../h/vm.h
vmmem.o: ../h/file.h
vmmem.o: ../h/inode.h
vmmon.o: ../h/param.h
vmmon.o: ../h/systm.h
vmmon.o: ../h/dir.h
vmmon.o: ../h/user.h
vmmon.o: ../h/vmmon.h
vmpage.o: ../h/param.h
vmpage.o: ../h/systm.h
vmpage.o: ../h/inode.h
vmpage.o: ../h/dir.h
vmpage.o: ../h/user.h
vmpage.o: ../h/proc.h
vmpage.o: ../h/reg.h
vmpage.o: ../h/pte.h
vmpage.o: ../h/buf.h
vmpage.o: ../h/text.h
vmpage.o: ../h/mtpr.h
vmpage.o: ../h/cmap.h
vmpage.o: ../h/vm.h
vmpage.o: ../h/vmmon.h
vmpage.o: ../h/file.h
vmproc.o: ../h/param.h
vmproc.o: ../h/systm.h
vmproc.o: ../h/dir.h
vmproc.o: ../h/user.h
vmproc.o: ../h/proc.h
vmproc.o: ../h/mtpr.h
vmproc.o: ../h/pte.h
vmproc.o: ../h/map.h
vmproc.o: ../h/cmap.h
vmproc.o: ../h/text.h
vmproc.o: ../h/vm.h
vmpt.o: ../h/param.h
vmpt.o: ../h/systm.h
vmpt.o: ../h/dir.h
vmpt.o: ../h/user.h
vmpt.o: ../h/proc.h
vmpt.o: ../h/map.h
vmpt.o: ../h/mtpr.h
vmpt.o: ../h/pte.h
vmpt.o: ../h/cmap.h
vmpt.o: ../h/vm.h
vmpt.o: ../h/buf.h
vmpt.o: ../h/text.h
vmsched.o: ../h/param.h
vmsched.o: ../h/systm.h
vmsched.o: ../h/seg.h
vmsched.o: ../h/dir.h
vmsched.o: ../h/user.h
vmsched.o: ../h/proc.h
vmsched.o: ../h/text.h
vmsched.o: ../h/vm.h
vmsched.o: ../h/cmap.h
vmsubr.o: ../h/param.h
vmsubr.o: ../h/systm.h
vmsubr.o: ../h/dir.h
vmsubr.o: ../h/user.h
vmsubr.o: ../h/vm.h
vmsubr.o: ../h/proc.h
vmsubr.o: ../h/mtpr.h
vmsubr.o: ../h/pte.h
vmsubr.o: ../h/cmap.h
vmsubr.o: ../h/inode.h
vmsubr.o: ../h/buf.h
vmsubr.o: ../h/text.h
vmswap.o: ../h/param.h
vmswap.o: ../h/systm.h
vmswap.o: ../h/dir.h
vmswap.o: ../h/user.h
vmswap.o: ../h/proc.h
vmswap.o: ../h/text.h
vmswap.o: ../h/map.h
vmswap.o: ../h/buf.h
vmswap.o: ../h/pte.h
vmswap.o: ../h/mtpr.h
vmswap.o: ../h/cmap.h
vmswap.o: ../h/vm.h
vmsys.o: ../h/param.h
vmsys.o: ../h/systm.h
vmsys.o: ../h/dir.h
vmsys.o: ../h/user.h
vmsys.o: ../h/proc.h
vmsys.o: ../h/reg.h
vmsys.o: ../h/file.h
vmsys.o: ../h/inode.h
vmsys.o: ../h/vm.h
vmsys.o: ../h/buf.h
vmsys.o: ../h/pte.h
vmsysent.o: ../h/param.h
vmsysent.o: ../h/systm.h
vp.o: ../h/param.h
vp.o: ../h/dir.h
vp.o: ../h/user.h
vp.o: ../h/buf.h
vp.o: ../h/systm.h
vp.o: ../h/map.h
vp.o: ../h/pte.h
vp.o: ../h/uba.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
