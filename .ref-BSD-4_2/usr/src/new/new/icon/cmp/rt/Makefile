CFLAGS = -O -w

all:		start.o pstart.o pstop.o Lib

Lib:		alc.o anycmp.o arith.o cplist.o csv.o ctype.o \
		cvcset.o cvint.o cvnum.o cvpos.o cvreal.o cvstr.o dblocks.o \
		defany.o defcset.o deffile.o defint.o defshort.o defstr.o \
		deref.o doasgn.o dump.o equiv.o exp.o fail.o floor.o gc.o \
		gcollect.o gcvt.o getstr.o hash.o init.o ldfps.o lexcmp.o \
		log.o mkint.o mkreal.o mksubs.o mon.o numcmp.o outimage.o \
		pow.o putstr.o qtos.o setbound.o suspend.o trace.o
	ar r ../bin/Lib $?
	touch Lib

start.o:	../h/defs.s
	cat ../h/defs.s start.s >t.c
	cc -E t.c | grep -v "^#" >t.s
	as -o start.o t.s
	rm -f t.c t.s
alc.o:		../h/rt.h ../h/gc.h ../h/record.h
anycmp.o:	../h/rt.h
arith.o:	../h/defs.s
cplist.o:	../h/rt.h
csv.o:		../h/defs.s
ctype.o:	../h/rt.h ../h/ctype.h
cvcset.o:	../h/rt.h
cvint.o:	../h/rt.h
cvnum.o:	../h/rt.h ../h/ctype.h
cvpos.o:	../h/rt.h
cvreal.o:	../h/rt.h
cvstr.o:	../h/rt.h
dblocks.o:	../h/rt.h
defany.o:	../h/rt.h
defcset.o:	../h/rt.h
deffile.o:	../h/rt.h
defint.o:	../h/rt.h
defshort.o:	../h/rt.h
defstr.o:	../h/rt.h
deref.o:	../h/rt.h
doasgn.o:	../h/rt.h
dump.o:		../h/rt.h ../h/gc.h
equiv.o:	../h/rt.h
exp.o:		../h/rt.h
fail.o:		../h/defs.s
floor.o:	../h/rt.h
gc.o:		../h/rt.h ../h/gc.h
gcollect.o:	../h/defs.s
gcvt.o:		../h/rt.h
getstr.o:	../h/rt.h
hash.o:		../h/rt.h
init.o:		../h/rt.h ../h/err.h
ldfps.o:	../h/defs.s
lexcmp.o:	../h/rt.h
log.o:		../h/rt.h
mkint.o:	../h/rt.h
mkreal.o:	../h/rt.h
mksubs.o:	../h/rt.h
numcmp.o:	../h/rt.h
outimage.o:	../h/rt.h ../h/record.h
pow.o:		../h/rt.h
putstr.o:	../h/rt.h
qtos.o:		../h/rt.h
suspend.o:	../h/defs.s
trace.o:	../h/rt.h

Listall:
	@pr *.[cs]
	@date >List

List:		start.s alc.c anycmp.c arith.s cplist.c csv.s ctype.c \
		cvcset.c cvint.c cvnum.c cvpos.c cvreal.c cvstr.c dblocks.c \
		defany.c defcset.c deffile.c defint.c defshort.c defstr.c \
		deref.c doasgn.c dump.c equiv.c exp.c fail.s floor.c gc.c \
		gcollect.s gcvt.c getstr.c hash.c init.c ldfps.s lexcmp.c \
		log.c mkint.c mkreal.c mksubs.c numcmp.c outimage.c pow.c \
		putstr.c qtos.c setbound.s suspend.s trace.c
	@pr $?
	@date >List

.s.o:
	cat ../h/defs.s $< >t.c
	cc -E t.c | grep -v "^#" >t.s
	as -o $@ t.s
	rm -f t.c t.s
clean:
	rm -f Lib *.o

dist-clean: clean
	rm -f `gcomp Makefile *.c *.s`
