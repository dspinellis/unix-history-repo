#	Makefile for a Fortran 77 compiler
#	running on the VAX, generating code for the VAX,
#	using the second pass of the Portable C compiler as code generator.

CFL = -DHERE=VAX -DTARGET=VAX -DFAMILY=PCC -DUCBVAXASM

CFLAGS = -O $(CFL)
LDFLAGS = -i

OBJECTS = main.o init.o gram.o lex.o proc.o equiv.o data.o \
	  expr.o exec.o intr.o io.o misc.o error.o put.o \
	  putpcc.o vax.o vaxx.o

compiler : f77 f77pass1
	touch compiler

f77 : driver.o vaxx.o
	cc -n driver.o vaxx.o -o f77
	@size f77

f77pass1 : $(OBJECTS)
	@echo LOAD
	@$(CC) $(LDFLAGS) $(OBJECTS) -o f77pass1
	@size f77pass1

gram.c:	gram.head gram.dcl gram.expr gram.exec gram.io tokdefs
	( sed <tokdefs "s/#define/%token/" ;\
		cat gram.head gram.dcl gram.expr gram.exec gram.io ) >gram.in
	$(YACC) $(YFLAGS) gram.in
	@echo "(expect 4 shift/reduce)"
	mv y.tab.c gram.c
	rm gram.in

tokdefs: tokens
	grep -n . <tokens | sed "s/\([^:]*\):\(.*\)/#define \2 \1/" >tokdefs
lex.o : tokdefs
driver.o $(OBJECTS)  : defs defines machdefs ftypes
driver.o : drivedefs
io.o : fio.h

machdefs : vaxdefs
	cp vaxdefs machdefs

put.o putpcc.o vax.o : pccdefs

install : /usr/bin/f77 /usr/lib/f77pass1

/usr/bin/f77 : f77
	strip f77
	@size f77 /usr/bin/f77
	cp f77 /usr/bin/f77

/usr/lib/f77pass1 : f77pass1
	strip f77pass1
	@size f77pass1 /usr/lib/f77pass1
	cp f77pass1 /usr/lib/f77pass1

cleanup:
	-rm gram.c *.o f77 f77pass1 tokdefs compiler
	du
