CFLAGS = -O -w

Lib:		asgn.o bang.o cat.o compl.o diff.o div.o eqv.o inter.o \
		lconcat.o lexeq.o lexge.o lexgt.o lexle.o lexlt.o lexne.o \
		minus.o mod.o mult.o neg.o neqv.o nonnull.o null.o number.o \
		numeq.o numge.o numgt.o numle.o numlt.o numne.o \
		plus.o power.o random.o rasgn.o refresh.o rswap.o sect.o \
		size.o subsc.o swap.o tabmat.o toby.o unioncs.o value.o
	ar r ../bin/Lib $?
	touch Lib

asgn.o:		../h/rt.h
bang.o:		../h/rt.h ../h/record.h
cat.o:		../h/rt.h
compl.o:	../h/rt.h
diff.o:		../h/rt.h
div.o:		../h/rt.h
eqv.o:		../h/rt.h
inter.o:	../h/rt.h
lconcat.o:	../h/rt.h
lexeq.o:	../h/rt.h
lexge.o:	../h/rt.h
lexgt.o:	../h/rt.h
lexle.o:	../h/rt.h
lexlt.o:	../h/rt.h
lexne.o:	../h/rt.h
minus.o:	../h/rt.h
mod.o:		../h/rt.h
mult.o:		../h/rt.h
neg.o:		../h/rt.h
neqv.o:		../h/rt.h
nonnull.o:	../h/rt.h
null.o:		../h/rt.h
number.o:	../h/rt.h
numeq.o:	../h/rt.h
numge.o:	../h/rt.h
numgt.o:	../h/rt.h
numle.o:	../h/rt.h
numlt.o:	../h/rt.h
numne.o:	../h/rt.h
plus.o:		../h/rt.h
power.o:	../h/rt.h
random.o:	../h/rt.h ../h/record.h
rasgn.o:	../h/rt.h
refresh.o:	../h/rt.h
rswap.o:	../h/rt.h
sect.o:		../h/rt.h
size.o:		../h/rt.h ../h/record.h
subsc.o:	../h/rt.h ../h/record.h
swap.o:		../h/rt.h
tabmat.o:	../h/rt.h
toby.o:		../h/rt.h
unioncs.o:	../h/rt.h
value.o:	../h/rt.h

Listall:
	@pr *.[cs]
	@date >List

List:		asgn.c bang.c cat.c compl.c diff.c div.c eqv.c inter.c \
		lconcat.c lexeq.c lexge.c lexgt.c lexle.c lexlt.c lexne.c \
		minus.c mod.c mult.c neg.c neqv.c nonnull.c null.c number.c \
		numeq.c numge.c numgt.c numle.c numlt.c numne.c \
		plus.c power.c random.c rasgn.c refresh.c rswap.c sect.c \
		size.c subsc.c swap.c tabmat.c toby.c unioncs.c value.c
	@pr $?
	@date >List

.s.o:
	as -o $@ ../h/defs.s $<
clean:
	rm -f Lib *.o

dist-clean: clean
	rm -f `gcomp Makefile *.c`
