CFLAGS = -O

Lib:		asgn.o bang.o cat.o compl.o diff.o div.o eqv.o inter.o \
		lconcat.o lexeq.o lexge.o lexgt.o lexle.o lexlt.o lexne.o \
		minus.o mod.o mult.o neg.o neqv.o nonnull.o null.o number.o \
		numeq.o numge.o numgt.o numle.o numlt.o numne.o \
		plus.o power.o random.o rasgn.o refresh.o rswap.o sect.o \
		size.o subsc.o swap.o tabmat.o toby.o unioncs.o value.o
		ar cr Lib $?
		ranlib Lib

asgn.o:		../h/rt.h ../h/config.h
bang.o:		../h/rt.h ../h/record.h ../h/config.h
cat.o:		../h/rt.h ../h/config.h
compl.o:	../h/rt.h ../h/config.h
diff.o:		../h/rt.h ../h/config.h
div.o:		../h/rt.h ../h/config.h
eqv.o:		../h/rt.h ../h/config.h
inter.o:	../h/rt.h ../h/config.h
lconcat.o:	../h/rt.h ../h/config.h
lexeq.o:	../h/rt.h ../h/config.h
lexge.o:	../h/rt.h ../h/config.h
lexgt.o:	../h/rt.h ../h/config.h
lexle.o:	../h/rt.h ../h/config.h
lexlt.o:	../h/rt.h ../h/config.h
lexne.o:	../h/rt.h ../h/config.h
minus.o:	../h/rt.h ../h/config.h
mod.o:		../h/rt.h ../h/config.h
mult.o:		../h/rt.h ../h/config.h
neg.o:		../h/rt.h ../h/config.h
neqv.o:		../h/rt.h ../h/config.h
nonnull.o:	../h/rt.h ../h/config.h
null.o:		../h/rt.h ../h/config.h
number.o:	../h/rt.h ../h/config.h
numeq.o:	../h/rt.h ../h/config.h
numge.o:	../h/rt.h ../h/config.h
numgt.o:	../h/rt.h ../h/config.h
numle.o:	../h/rt.h ../h/config.h
numlt.o:	../h/rt.h ../h/config.h
numne.o:	../h/rt.h ../h/config.h
plus.o:		../h/rt.h ../h/config.h
power.o:	../h/rt.h ../h/config.h
random.o:	../h/rt.h ../h/record.h ../h/config.h
rasgn.o:	../h/rt.h ../h/config.h
refresh.o:	../h/rt.h ../h/config.h
rswap.o:	../h/rt.h ../h/config.h
sect.o:		../h/rt.h ../h/config.h
size.o:		../h/rt.h ../h/record.h ../h/config.h
subsc.o:	../h/rt.h ../h/record.h ../h/config.h
swap.o:		../h/rt.h ../h/config.h
tabmat.o:	../h/rt.h ../h/config.h
toby.o:		../h/rt.h ../h/config.h
unioncs.o:	../h/rt.h ../h/config.h
value.o:	../h/rt.h ../h/config.h

Listall:
		@pr *.c
		@date >List

List:		asgn.c bang.c cat.c compl.c diff.c div.c eqv.c inter.c \
		lconcat.c lexeq.c lexge.c lexgt.c lexle.c lexlt.c lexne.c \
		minus.c mod.c mult.c neg.c neqv.c nonnull.c null.c number.c \
		numeq.c numge.c numgt.c numle.c numlt.c numne.c \
		plus.c power.c random.c rasgn.c refresh.c rswap.c sect.c \
		size.c subsc.c swap.c tabmat.c toby.c unioncs.c value.c
		@pr $?
		@date >List

Clean:
		rm -f Lib *.o

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm -f `gcomp Makefile *.c`
