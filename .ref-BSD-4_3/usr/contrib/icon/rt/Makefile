CFLAGS = -O

Lib:		addmem.o alc.o anycmp.o arith.o cplist.o csv.o ctype.o \
		cvcset.o cvint.o cvnum.o cvpos.o cvreal.o cvstr.o dblocks.o \
		defany.o defcset.o deffile.o defint.o defshort.o defstr.o \
		deref.o doasgn.o dump.o equiv.o exp.o fail.o floor.o gc.o \
		gcollect.o gcvt.o getstr.o hash.o host.o ldfps.o lexcmp.o \
		locate.o log.o memb.o mkint.o mkreal.o mksubs.o numcmp.o \
		outimage.o pow.o putstr.o qtos.o setbound.o strprc.o \
		suspend.o sweep.o trace.o
		ar rc Lib $?
		ranlib Lib

addmem.o:	../h/rt.h ../h/config.h
alc.o:		../h/rt.h ../h/gc.h ../h/record.h ../h/config.h
anycmp.o:	../h/rt.h ../h/config.h
arith.o:	../h/defs.s ../h/config.h
cplist.o:	../h/rt.h ../h/config.h
csv.o:		../h/defs.s ../h/config.h
ctype.o:	../h/rt.h ../h/ctype.h ../h/config.h
cvcset.o:	../h/rt.h ../h/config.h
cvint.o:	../h/rt.h ../h/config.h
cvnum.o:	../h/rt.h ../h/ctype.h ../h/config.h
cvpos.o:	../h/rt.h ../h/config.h
cvreal.o:	../h/rt.h ../h/config.h
cvstr.o:	../h/rt.h ../h/config.h
dblocks.o:	../h/rt.h ../h/config.h
defany.o:	../h/rt.h ../h/config.h
defcset.o:	../h/rt.h ../h/config.h
deffile.o:	../h/rt.h ../h/config.h
defint.o:	../h/rt.h ../h/config.h
defshort.o:	../h/rt.h ../h/config.h
defstr.o:	../h/rt.h ../h/config.h
deref.o:	../h/rt.h ../h/config.h
doasgn.o:	../h/rt.h ../h/config.h
dump.o:		../h/rt.h ../h/gc.h ../h/config.h
equiv.o:	../h/rt.h ../h/config.h
exp.o:		../h/rt.h ../h/config.h
fail.o:		../h/defs.s ../h/config.h
floor.o:	../h/rt.h ../h/config.h
gc.o:		../h/rt.h ../h/gc.h ../h/config.h
gcollect.o:	../h/defs.s ../h/config.h
gcvt.o:		../h/rt.h ../h/config.h
getstr.o:	../h/rt.h ../h/config.h
hash.o:		../h/rt.h ../h/config.h
host.o:		../h/rt.h ../h/config.h
ldfps.o:	../h/defs.s ../h/config.h
lexcmp.o:	../h/rt.h ../h/config.h
locate.o:	../h/rt.h ../h/config.h
log.o:		../h/rt.h ../h/config.h
memb.o:		../h/rt.h ../h/config.h
mkint.o:	../h/rt.h ../h/config.h
mkreal.o:	../h/rt.h ../h/config.h
mksubs.o:	../h/rt.h ../h/config.h
numcmp.o:	../h/rt.h ../h/config.h
outimage.o:	../h/rt.h ../h/record.h ../h/config.h
pow.o:		../h/rt.h ../h/config.h
putstr.o:	../h/rt.h ../h/config.h
qtos.o:		../h/rt.h ../h/config.h
setbound.o:	../h/defs.s ../h/config.h
strprc.o:	../h/rt.h ../h/pnames.h ../h/config.h ../h/pdef.h
suspend.o:	../h/defs.s ../h/config.h
sweep.o:	../h/rt.h ../h/gc.h ../h/config.h
trace.o:	../h/rt.h ../h/config.h

Listall:
		@pr *.[cs]
		@date >List

List:		addmem.c alc.c anycmp.c arith.s cplist.c csv.s ctype.c \
		cvcset.c cvint.c cvnum.c cvpos.c cvreal.c cvstr.c dblocks.c \
		defany.c defcset.c deffile.c defint.c defshort.c defstr.c \
		deref.c doasgn.c dump.c equiv.c exp.c fail.s floor.c gc.c \
		gcollect.s gcvt.c getstr.c hash.c host.c ldfps.s lexcmp.c \
		locate.c log.c memb.c mkint.c mkreal.c mksubs.c numcmp.c \
		outimage.c pow.c putstr.c qtos.c setbound.s suspend.s \
		sweep.c trace.c
		@pr $?
		@date >List

.s.o:
		cat ../h/defs.s $< >t.c
		cc -E t.c | egrep -v "(^#|^$$)" >t.s
		as -o $@ t.s
		rm -f t.c t.s
Clean:
		rm -f Lib *.o

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm -f `gcomp Makefile *.c *.s`
