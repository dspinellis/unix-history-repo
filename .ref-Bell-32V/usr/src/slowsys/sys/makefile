CFLAGS=-O
AHEADS = ../h/mtpr.m ../h/trap.m ../h/psl.m ../h/pcb.m ../h/clock.m \
		../h/cons.m ../h/page.m ../h/mba.m ../h/uba.m
FILES=\
locore.o \
acct.o \
alloc.o \
bio.o \
clock.o \
conf.o \
cons.o \
dkleave.o \
dsort.o \
dz.o \
fio.o \
hp.o \
ht.o \
iget.o \
machdep.o \
main.o \
malloc.o \
mba.o \
mem.o \
mx1.o \
mx2.o \
nami.o \
partab.o \
pipe.o \
prf.o \
prim.o \
rdwri.o \
sig.o \
slp.o \
subr.o \
sys.o \
sys1.o \
sys2.o \
sys3.o \
sys4.o \
sysent.o \
tdump.o \
text.o \
trap.o \
tty.o \
uba.o \
univec.o \
ureg.o 


unix: $(FILES)
	sh loadall


dz.o hp.o ht.o mba.o tdump.o uba.o:
	cc -c -S $*.c
	/lib/c2 -i $*.s $*.os
	as -o $*.o $*.os
	rm $*.s $*.os

locore.o: $(AHEADS)
	cat $(AHEADS) locore.s | as -o locore.o


acct.o:../h/param.h
acct.o:../h/systm.h
acct.o:../h/acct.h
acct.o:../h/dir.h
acct.o:../h/user.h
acct.o:../h/inode.h
acct.o:../h/proc.h
acct.o:../h/seg.h
alloc.o:../h/param.h
alloc.o:../h/systm.h
alloc.o:../h/mount.h
alloc.o:../h/filsys.h
alloc.o:../h/fblk.h
alloc.o:../h/conf.h
alloc.o:../h/buf.h
alloc.o:../h/inode.h
alloc.o:../h/ino.h
alloc.o:../h/dir.h
alloc.o:../h/user.h
bio.o:../h/param.h
bio.o:../h/systm.h
bio.o:../h/dir.h
bio.o:../h/user.h
bio.o:../h/buf.h
bio.o:../h/conf.h
bio.o:../h/proc.h
bio.o:../h/seg.h
clock.o:../h/param.h
clock.o:../h/systm.h
clock.o:../h/callo.h
clock.o:../h/seg.h
clock.o:../h/dir.h
clock.o:../h/user.h
clock.o:../h/proc.h
clock.o:../h/reg.h
clock.o:../h/psl.h
conf.o:../h/param.h
conf.o:../h/systm.h
conf.o:../h/buf.h
conf.o:../h/tty.h
conf.o:../h/conf.h
conf.o:../h/proc.h
conf.o:../h/text.h
conf.o:../h/dir.h
conf.o:../h/user.h
conf.o:../h/file.h
conf.o:../h/inode.h
conf.o:../h/acct.h
conf.o:../h/mba.h
cons.o:../h/param.h
cons.o:../h/conf.h
cons.o:../h/dir.h
cons.o:../h/user.h
cons.o:../h/tty.h
cons.o:../h/systm.h
cons.o:../h/cons.h
cons.o:../h/mtpr.h
dkleave.o:../h/param.h
dkleave.o:../h/buf.h
dsort.o:../h/param.h
dsort.o:../h/systm.h
dsort.o:../h/buf.h
dz.o:../h/param.h
dz.o:../h/tty.h
dz.o:../h/uba.h
dz.o:../h/proc.h
dz.o:../h/dir.h
dz.o:../h/file.h
dz.o:../h/inode.h
dz.o:../h/user.h
dz.o:../h/conf.h
fio.o:../h/param.h
fio.o:../h/systm.h
fio.o:../h/dir.h
fio.o:../h/user.h
fio.o:../h/filsys.h
fio.o:../h/file.h
fio.o:../h/conf.h
fio.o:../h/inode.h
fio.o:../h/reg.h
fio.o:../h/acct.h
hp.o:../h/param.h
hp.o:../h/uba.h
hp.o:../h/systm.h
hp.o:../h/buf.h
hp.o:../h/conf.h
hp.o:../h/dir.h
hp.o:../h/user.h
hp.o:../h/map.h
hp.o:../h/mba.h
ht.o:../h/param.h
ht.o:../h/systm.h
ht.o:../h/buf.h
ht.o:../h/conf.h
ht.o:../h/dir.h
ht.o:../h/user.h
ht.o:../h/uba.h
ht.o:../h/map.h
ht.o:../h/mba.h
iget.o:../h/param.h
iget.o:../h/systm.h
iget.o:../h/mount.h
iget.o:../h/dir.h
iget.o:../h/user.h
iget.o:../h/inode.h
iget.o:../h/ino.h
iget.o:../h/filsys.h
iget.o:../h/conf.h
iget.o:../h/buf.h
machdep.o:../h/param.h
machdep.o:../h/systm.h
machdep.o:../h/acct.h
machdep.o:../h/dir.h
machdep.o:../h/user.h
machdep.o:../h/inode.h
machdep.o:../h/proc.h
machdep.o:../h/seg.h
machdep.o:../h/uba.h
machdep.o:../h/map.h
machdep.o:../h/reg.h
machdep.o:../h/mtpr.h
machdep.o:../h/clock.h
main.o:../h/param.h
main.o:../h/systm.h
main.o:../h/dir.h
main.o:../h/user.h
main.o:../h/filsys.h
main.o:../h/mount.h
main.o:../h/uba.h
main.o:../h/map.h
main.o:../h/proc.h
main.o:../h/inode.h
main.o:../h/seg.h
main.o:../h/conf.h
main.o:../h/buf.h
main.o:../h/mtpr.h
main.o:../h/page.h
main.o:../h/clock.h
malloc.o:../h/param.h
malloc.o:../h/systm.h
malloc.o:../h/uba.h
malloc.o:../h/map.h
mba.o:../h/param.h
mba.o:../h/buf.h
mba.o:../h/conf.h
mba.o:../h/systm.h
mba.o:../h/dir.h
mba.o:../h/user.h
mba.o:../h/proc.h
mba.o:../h/seg.h
mba.o:../h/page.h
mba.o:../h/uba.h
mba.o:../h/map.h
mba.o:../h/mba.h
mba.o:../h/mtpr.h
mem.o:../h/param.h
mem.o:../h/dir.h
mem.o:../h/user.h
mem.o:../h/conf.h
mem.o:../h/buf.h
mem.o:../h/systm.h
mem.o:../h/page.h
mem.o:../h/mtpr.h
mx1.o:../h/param.h
mx1.o:../h/systm.h
mx1.o:../h/dir.h
mx1.o:../h/user.h
mx1.o:../h/reg.h
mx1.o:../h/proc.h
mx1.o:../h/inode.h
mx1.o:../h/file.h
mx1.o:../h/tty.h
mx1.o:../h/mx.h
mx1.o:../h/conf.h
mx2.o:../h/param.h
mx2.o:../h/systm.h
mx2.o:../h/dir.h
mx2.o:../h/user.h
mx2.o:../h/proc.h
mx2.o:../h/inode.h
mx2.o:../h/file.h
mx2.o:../h/tty.h
mx2.o:../h/mx.h
mx2.o:../h/conf.h
mx2.o:../h/buf.h
nami.o:../h/param.h
nami.o:../h/systm.h
nami.o:../h/inode.h
nami.o:../h/mount.h
nami.o:../h/dir.h
nami.o:../h/user.h
nami.o:../h/buf.h
pipe.o:../h/param.h
pipe.o:../h/systm.h
pipe.o:../h/dir.h
pipe.o:../h/user.h
pipe.o:../h/inode.h
pipe.o:../h/file.h
pipe.o:../h/reg.h
prf.o:../h/param.h
prf.o:../h/systm.h
prf.o:../h/seg.h
prf.o:../h/buf.h
prf.o:../h/conf.h
prim.o:../h/param.h
prim.o:../h/tty.h
prim.o:../h/systm.h
prim.o:../h/conf.h
prim.o:../h/buf.h
rdwri.o:../h/param.h
rdwri.o:../h/systm.h
rdwri.o:../h/inode.h
rdwri.o:../h/dir.h
rdwri.o:../h/user.h
rdwri.o:../h/buf.h
rdwri.o:../h/conf.h
sig.o:../h/param.h
sig.o:../h/systm.h
sig.o:../h/dir.h
sig.o:../h/user.h
sig.o:../h/proc.h
sig.o:../h/inode.h
sig.o:../h/reg.h
sig.o:../h/text.h
sig.o:../h/seg.h
sig.o:../h/mtpr.h
sig.o:../h/page.h
slp.o:../h/param.h
slp.o:../h/systm.h
slp.o:../h/dir.h
slp.o:../h/user.h
slp.o:../h/proc.h
slp.o:../h/text.h
slp.o:../h/uba.h
slp.o:../h/map.h
slp.o:../h/file.h
slp.o:../h/inode.h
slp.o:../h/buf.h
subr.o:../h/param.h
subr.o:../h/systm.h
subr.o:../h/conf.h
subr.o:../h/inode.h
subr.o:../h/dir.h
subr.o:../h/user.h
subr.o:../h/buf.h
sys.o:../h/param.h
sys.o:../h/conf.h
sys.o:../h/dir.h
sys.o:../h/user.h
sys.o:../h/tty.h
sys.o:../h/proc.h
sys1.o:../h/param.h
sys1.o:../h/systm.h
sys1.o:../h/uba.h
sys1.o:../h/map.h
sys1.o:../h/dir.h
sys1.o:../h/user.h
sys1.o:../h/proc.h
sys1.o:../h/buf.h
sys1.o:../h/reg.h
sys1.o:../h/inode.h
sys1.o:../h/seg.h
sys1.o:../h/acct.h
sys1.o:../h/page.h
sys2.o:../h/param.h
sys2.o:../h/systm.h
sys2.o:../h/dir.h
sys2.o:../h/user.h
sys2.o:../h/reg.h
sys2.o:../h/file.h
sys2.o:../h/inode.h
sys3.o:../h/param.h
sys3.o:../h/systm.h
sys3.o:../h/mount.h
sys3.o:../h/ino.h
sys3.o:../h/reg.h
sys3.o:../h/buf.h
sys3.o:../h/filsys.h
sys3.o:../h/dir.h
sys3.o:../h/user.h
sys3.o:../h/inode.h
sys3.o:../h/file.h
sys3.o:../h/conf.h
sys3.o:../h/stat.h
sys4.o:../h/param.h
sys4.o:../h/systm.h
sys4.o:../h/dir.h
sys4.o:../h/user.h
sys4.o:../h/reg.h
sys4.o:../h/inode.h
sys4.o:../h/proc.h
sys4.o:../h/clock.h
sys4.o:../h/mtpr.h
sys4.o:../h/timeb.h
sysent.o:../h/param.h
sysent.o:../h/systm.h
text.o:../h/param.h
text.o:../h/systm.h
text.o:../h/uba.h
text.o:../h/map.h
text.o:../h/dir.h
text.o:../h/user.h
text.o:../h/proc.h
text.o:../h/text.h
text.o:../h/inode.h
text.o:../h/buf.h
text.o:../h/seg.h
text.o:../h/page.h
trap.o:../h/param.h
trap.o:../h/systm.h
trap.o:../h/dir.h
trap.o:../h/user.h
trap.o:../h/proc.h
trap.o:../h/reg.h
trap.o:../h/seg.h
trap.o:../h/trap.h
trap.o:../h/psl.h
tty.o:../h/param.h
tty.o:../h/systm.h
tty.o:../h/dir.h
tty.o:../h/user.h
tty.o:../h/tty.h
tty.o:../h/proc.h
tty.o:../h/chan.h
tty.o:../h/inode.h
tty.o:../h/file.h
tty.o:../h/reg.h
tty.o:../h/conf.h
uba.o:../h/param.h
uba.o:../h/uba.h
uba.o:../h/map.h
univec.o:../h/param.h
ureg.o:../h/param.h
ureg.o:../h/systm.h
ureg.o:../h/dir.h
ureg.o:../h/user.h
ureg.o:../h/proc.h
ureg.o:../h/text.h
ureg.o:../h/seg.h
ureg.o:../h/mtpr.h
ureg.o:../h/page.h
v45lnk.o:../h/param.h
v45lnk.o:../h/dir.h
v45lnk.o:../h/user.h
v45lnk.o:../h/proc.h
v45lnk.o:../h/uba.h
v45lnk.o:../h/buf.h
